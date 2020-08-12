#!/usr/bin/env python
# -*- coding: utf-8 -*-

import json, pyodbc, re, requests
from flask_limiter import Limiter
from flask import Flask, request, make_response
from werkzeug.exceptions import NotFound, BadRequest
from functools import wraps

#------------------------CONTROLE-DE-ACESSO---------------------------

app = Flask(__name__)
limit = Limiter(app, strategy='fixed-window-elastic-expiry').shared_limit('480 per minute, 40 per second', 'global', error_message='Try again later.')

def valida_curso(curso, antigo):
    try: assert re.match('^[a-z_]*$', curso)
    except: raise NotFound

    if antigo: command = 'select * from Curso where NomeSchema = "%s"' % curso
    else: command = 'select * from preanalytics2015.cursos as c where c.schema = "%s"' % curso
    if len(get_rows(command)) == 0: raise NotFound

class interface:
    def __init__(self, url):
        self.url = url

    def __call__(self, func):
        @wraps(limit(func))
        def decorator(*args, **kargs):
            if 'curso' in kargs: valida_curso(kargs['curso'], antigo=False)
            if 'curso' in request.args: valida_curso(request.args['curso'], antigo=True)
            return format_response(func(*args, **kargs))

        return app.route(self.url)(decorator)

@app.after_request
def add_headers(response):
    response.headers['Access-Control-Allow-Origin'] = '*'
    response.headers['Access-Control-Allow-Headers'] = 'Authorization'
    response.headers['Access-Control-Allow-Methods'] = 'GET, OPTIONS'
    return response

#-------------------------INTERFACE-WEB-------------------------------

execfile('apiRestOld.py')

@interface('/cursos_2015')
def cursos_novos():
    command = 'select # from preanalytics2015.cursos where disponivel=true'
    cols = ['schema', 'campus', 'nome_comum']
    return retrieve(command, cols)

@interface('/<curso>')
def info_curso(curso):
    command = 'select # from preanalytics2015.cursos where `schema`="' + curso + '"'
    cols = ['codigo_curso', 'curso', 'nome_comum', 'campus', 'codigo_emec', 'turno', 'horas', 'tempo_minimo', 'vagas_primeira', 'vagas_segunda', 'ato_normativo']
    return retrieve(command, cols, single=True)

@interface('/<curso>/disciplinas')
def disciplinas(curso):
    command = 'select # from ' + curso + '.disciplinas'
    cols = ['codigo_disciplina', 'disciplina', 'tipo', 'codigo_departamento', 'semestre', 'horas', 'creditos']
    table = retrieve(command, cols)

    mapa = {}
    for d in table:
        d['pre_requisitos'] = list()
        d['pos_requisitos'] = list()
        mapa[d['codigo_disciplina']] = d

    command = 'select # from ' + curso + '.pre_requisitos'
    cols = ['codigo_disciplina', 'codigo_prerequisito']
    table = retrieve(command, cols)

    for p in table:
        mapa[p['codigo_disciplina']]['pre_requisitos'].append(p['codigo_prerequisito'])
        mapa[p['codigo_prerequisito']]['pos_requisitos'].append(p['codigo_disciplina'])

    result = sorted(mapa.values(), key=lambda x:x['semestre'])
    return result

@interface('/<curso>/taxa-sucesso')
def taxa_sucesso(curso):
    command = 'select # from ' + curso + '.aprovacoes'
    cols = ['codigo_disciplina', 'aprovados', 'total', 'periodo']
    return retrieve(command, cols)

@interface('/<curso>/taxa-sucesso/periodos')
def taxa_sucesso_periodos(curso):
    command = 'select min(periodo), max(periodo) from ' + curso + '.aprovacoes'
    cols = ['min_periodo', 'max_periodo']
    return retrieve(command, cols, single=True)

@interface('/<curso>/correlacao')
def correlacao(curso):
    param = {'periodo_inicial' : 0.1, 'periodo_final' : 3000.2, 'schema': '"%s"' % curso}
    data = open_cpu('precor', 'calcula_correlacao/json', param).json()

    disciplinas = []
    correlacoes = []

    index = {}
    index[...] = 0

    def register(d):
        disciplinas.append(d)
        index[d['nome']] = index[...]
        index[...] += 1

    def get_indice(d):
        if d not in disciplinas: register(d)
        return index[d['nome']]

    for relation in data:
        disciplina_A = {'nome': relation['disciplina_A'] , 'periodo': relation['semestre_A']}
        disciplina_B = {'nome': relation['disciplina_B'] , 'periodo': relation['semestre_B']}
        correlacoes.append({'source': get_indice(disciplina_A), 'target': get_indice(disciplina_B), 'valor': relation['correlacao']})

    return {'disciplinas': disciplinas, 'correlacoes': correlacoes}

@interface('/<curso>/recomendacao')
def recomendacao(curso):
    escolhas = request.args.get('disciplinas')
    historico = request.args.get('historico')
    nao_cursei = request.args.get('nao_cursei')

    try:
        assert re.match('\[(\d{7}(,\d{7})*)?]$', escolhas)
        assert re.match('\[(\d{7}(,\d{7})*)?]$', historico)
    except:
        raise BadRequest

    escolhas = json.loads(escolhas)
    historico = json.loads(historico)
    nao_cursei = json.loads(nao_cursei)

    def list2str(A):
        return ','.join(map(str, A))

    def disciplinas_recomendadas(curso, historico, escolhas, nao_cursei):
        command = 'select MAX(semestre) from ' + curso + '.disciplinas where codigo_disciplina in (%s)' % list2str(historico + escolhas)
        cols = ['periodo']

        periodo = retrieve(command, cols, single=True)['periodo']

        param = {
            'historico_': 'c(%s)' % list2str(historico),
            'disciplinas': 'c(%s)' % list2str(escolhas + nao_cursei),
            'course_name': '"%s"' % curso,
            'p': periodo
        }

        response = open_cpu('recomendacao', 'recomenda/json', param).text
        return json.loads(response)

    return disciplinas_recomendadas(curso, historico, escolhas, nao_cursei)

@interface('/<curso>/analise')
def analise(curso):
    escolhas = request.args.get('escolhas')
    historico = request.args.get('historico')

    try:
        assert re.match('\[(\d{7}(,\d{7})*)?]$', escolhas)
        assert re.match('\[(\d{7}(,\d{7})*)?]$', historico)
    except:
        raise BadRequest

    escolhas = json.loads(escolhas)
    historico = json.loads(historico)

    def list2str(A):
        return ','.join(map(str, A))

    def taxa_complecao(historico, curso):
        command = 'select COUNT(*) from ' + curso + '.disciplinas as disc WHERE disc.tipo in (\'Obrigatoria\',\'Complementar\')'
        total = get_rows(command)[0][0]

        return len(historico) / float(total)

    def frequencia_matricula(escolhas, curso):
        escolhas = sorted(escolhas)
        escolhas = list2str(escolhas)

        command = 'select count(*) as frequencia from (select h.matricula, h.periodo, GROUP_CONCAT(h.codigo_disciplina ORDER BY h.codigo_disciplina ASC SEPARATOR ",") as set_disc from ' +  curso + '.historico h,' + curso + '.disciplinas d where d.codigo_disciplina = h.codigo_disciplina and d.tipo in ("Complementar" , "Obrigatoria") group by h.matricula, h.periodo) tabela where tabela.set_disc = "' + escolhas + '" group by tabela.set_disc order by frequencia'
        cols = ['frequencia']

        resultado = retrieve(command, cols)

        if len(resultado) == 0: return 0
        return resultado[0]['frequencia']

    def probabilidade_matricula(historico, escolhas, curso):
        if 'ciencia_da_computacao_d_cg' != curso and 'engenharia_eletrica_cg' != curso:
            return None

        command = 'select MAX(semestre) from ' + curso + '.disciplinas where codigo_disciplina in (%s)' % list2str(historico + escolhas)
        cols = ['periodo']

        periodo = retrieve(command, cols, single=True)['periodo']

        param = {
            'historico_': 'c(%s)' % list2str(historico),
            'matricula_atual_': 'c(%s)' % list2str(escolhas),
            'course_name': '"%s"' % curso,
            'p': periodo
        }

        response = open_cpu('redes', 'pegar_prob_da_rede/print', param).text
        return float(response.split()[1])

    def dificuldade(escolhas, curso):
        command = 'select # from preanalytics2015.cursos where `schema` = "%s"' % curso
        cols = ['codigo_curso']
        codigo_curso = retrieve(command, cols, True)['codigo_curso']

        command = 'select DISTINCT(#) from preanalytics2015.disciplinas where codigo_curso = %d' % codigo_curso
        cols = ['codigo_disciplina']
        disciplinas = retrieve(command, cols)

        command = 'select AVG(media) from ' + curso + '.historico '
        cra = get_rows(command)[0][0]

        command = 'select AVG(media) from ' + curso + '.historico where codigo_disciplina in (%s)' % list2str(escolhas)
        media_set = get_rows(command)[0][0]

        disciplinas = [d['codigo_disciplina'] for d in disciplinas]
        n_disc = len(escolhas)

        names = list2str(disciplinas) + ',n_disc,media_set,cra'
        values = ','.join('TRUE' if d in escolhas else 'FALSE' for d in disciplinas) + ',%d,%f,%f' % (n_disc, media_set, cra)
        param = {'names': '"%s"' % names, 'values': '"%s"' % values, 'course_name': '"%s"' % curso}

        return open_cpu('termometro', 'make_prediction/json', param).json()[0]

    return {
        'risco_reprovacao': dificuldade(escolhas, curso),
        'taxa_complecao': taxa_complecao(historico, curso),
        'frequencia_matricula': frequencia_matricula(escolhas, curso),
        'probabilidade_matricula': probabilidade_matricula(historico, escolhas, curso)
    }

@interface('/<curso>/formandos')
def formandos(curso):
    command = 'select A.periodo, ingressos, ifnull(formandos, 0) formandos from (select periodo_ingressao periodo, count(id_aluno) ingressos from ' + curso + '.alunos group by periodo_ingressao) A left join (select periodo_ingressao periodo, count(id_aluno) formandos from ' + curso + '.alunos where codigo_evasao in (1, 20) group by periodo_ingressao) B on A.periodo = B.periodo'
    cols = ['periodo', 'ingressos', 'formandos']

    return retrieve(command, cols)

@interface('/<curso>/estatisticas')
def estatisticas(curso):
	def situacao():
		command = 'select codigo_evasao, count(codigo_evasao) from ' + curso + '.alunos where codigo_evasao in (0, 1) group by codigo_evasao'
		cols = ['codigo_evasao', 'quant']

		return retrieve(command, cols)

	def taxa_de_aprovacao():
		command = 'select sum(situacao = "Aprovado") / sum(situacao != "Trancado") from ' + curso + '.historico'
		cols = ['taxa_de_aprovacao']

		return float(retrieve(command, cols, True)['taxa_de_aprovacao'])

	alunos = situacao()

	return {
		'regulares': alunos[0]['quant'],
		'graduados': alunos[1]['quant'],
		'taxa_de_aprovacao': taxa_de_aprovacao() * 100
	}

#-------------------------PROCESSAMENTO-------------------------------

def create_connection():
    return pyodbc.connect('DSN=MyPRE; CHARSET=UTF8; UID=api-rest; PWD=restingplebeu')

def open_cpu(package, method, params):
   return requests.post('http://pre-ocpu/ocpu/library/' + package + '/R/' + method, data=params)

def retrieve(command, cols, single=False):
    cols_escaped = ['`%s`' % s for s in cols]
    cols_joined = str.join(', ', cols_escaped)
    command = command.replace('#', cols_joined)
    return make_json(get_rows(command), cols, single)

def get_rows(command):
    cnxn = create_connection()
    cursor = cnxn.cursor()
    cursor.execute(command)
    rows = cursor.fetchall()
    cnxn.close()
    return rows

def make_json(rows, cols, single):
    table = []
    for row in rows:
        celula = {}
        for i in range(len(cols)):
            celula[cols[i]] = row[i]
        table.append(celula)
    return table[0] if single else table

def format_response(response):
    response = json.dumps(response).encode('utf-8')
    response = make_response(response)
    response.headers['Content-Type'] = 'application/json; charset=utf-8'
    response.headers['Cache-Control'] = 'max-age=86400'
    return response

#--------------------------------MAIN---------------------------------

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5005)
