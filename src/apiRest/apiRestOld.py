# -*- coding: utf-8 -*-

@interface('/cursos')
def cursos():
    command = 'select * from Curso'
    cols = ['NomeSchema', 'NomeCurso']
    return retrieve(command, cols)

@interface('/disciplinasPorPeriodo')
def disciplinasPorPeriodo():
    command = 'select g.CodigoDisciplina, g.Periodo, d.NomeComum from ' + request.args['curso'] + '.GradeDisciplinasPorPeriodo as g, ' + request.args['curso'] + '.Disciplina as d where d.CodigoDisciplina = g.CodigoDisciplina and d.Obrigatoria = 1 order by g.Periodo'
    cols = ['codigo', 'periodo', 'disciplina']
    return retrieve(command, cols)

@interface('/preRequisito')
def preRequisito():
    command = 'select * from ' + request.args['curso'] + '.PreRequisitosDisciplina'
    cols = ['codigo', 'codigoPreRequisito']
    return retrieve(command, cols)

@interface('/maioresFrequencias')
def maioresFrequencias():
    command = 'select m.CodigoDisciplina, d.NomeComum, m.PeriodoMaisFreq1st, m.FreqRelativa1st, m.PeriodoMaisFreq2nd, m.FreqRelativa2nd, m.PeriodoMaisFreq3rd ,m.FreqRelativa3rd, m.TotalDeAlunosPorDisciplina from ' + request.args['curso'] + '.MaioresFrequenciasPorDisciplina m, ' + request.args['curso'] + '.Disciplina d where d.CodigoDisciplina = m.CodigoDisciplina and d.Obrigatoria = 1 order by m.PeriodoMaisFreq1st'
    cols = ['codigo', 'disciplina', 'periodoMaisFreq1st', 'freqRelativa1st', 'periodoMaisFreq2nd', 'freqRelativa2nd', 'periodoMaisFreq3rd', 'freqRelativa3rd', 'totalDeAlunos']
    return retrieve(command, cols)

@app.route('/reprovacoesSlider')
def reprovacoesSliderDefault():
    return reprovacoesSlider('2013.1', '2013.1')

@interface('/reprovacoesSlider/<periodo_inicial>/<periodo_final>')
def reprovacoesSlider(periodo_inicial, periodo_final):
    try:
        assert re.match('^\d*\.[1-2]$', periodo_inicial) and re.match('^\d*\.[1-2]$', periodo_final)
        assert float(periodo_inicial) <= float(periodo_final)
    except: raise BadRequest

    command = 'select d.NomeComum, r.CodigoDisciplina, r.ReprovacaoAbsoluta, r.ReprovacaoRelativa, r.TotalDeAlunos, r.PeriodoInicial, r.PeriodoFinal from ' + request.args['curso'] + '.Disciplina d, ' + request.args['curso'] + '.ReprovacoesSlider r where r.CodigoDisciplina = d.CodigoDisciplina and d.Obrigatoria = 1 and (r.PeriodoInicial = %s and r.PeriodoFinal = %s)' % (periodo_inicial, periodo_final)
    cols = ['disciplina', 'codigo', 'reprovacaoAbsoluta', 'reprovacaoRelativa', 'totalDeAlunos', 'PeriodoInicial', 'PeriodoFinal']
    return retrieve(command, cols)

@interface('/periodosReprovacoesSlider')
def periodosReprovacoesSlider():
    command = 'select distinct PeriodoInicial from ' + request.args['curso'] + '.ReprovacoesSlider'
    cols = ['Periodos']
    return retrieve(command, cols)

@app.route('/correlacoes')
def correlacoesDefault():
    return correlacoes(0.5)

@interface('/correlacoes/<filtro>')
def correlacoes(filtro):
    try:
        filtro = float(filtro)
        if not (0.0 <= filtro <= 1.0):
            raise ValueError
    except:
        filtro = 0.5

    command = 'select d1.NomeComum, c.CodDisciplina1, d2.NomeComum, c.CodDisciplina2, c.Correlacao from ' + request.args['curso'] + '.CorrelacaoDisciplinasPorNotas c, ' + request.args['curso'] + '.Disciplina d1, ' + request.args['curso'] + '.Disciplina d2 where d1.CodigoDisciplina = c.CodDisciplina1 and d2.CodigoDisciplina = c.CodDisciplina2 and d1.Obrigatoria = 1 and d2.Obrigatoria = 1 and (c.Correlacao >= %f or c.Correlacao <= %f)' % (filtro, -filtro)
    cols = ['disciplina1', 'codigo1', 'disciplina2', 'codigo2', 'correlacao']
    return retrieve(command, cols)

@interface('/evasao/evasaoRelativa')
def evasaoRelativa():
    command = 'select * from ' + request.args['curso'] + '.PeriodosRelativoEvasao'
    cols = ['PERIODO_RELATIVO', 'QUANT_EVASORES']
    return retrieve(command, cols)

@interface('/evasao/taxaEvasao')
def taxaEvasao():
    command = 'select * from ' + request.args['curso'] + '.TaxaEvasao'
    cols = ['Periodo', 'Taxa']
    return retrieve(command, cols)

@interface('/concluintes')
def concluintes():
    command = 'select * from ' + request.args['curso'] + '.Concluinte'
    cols = ['PERIODO', 'QUANT_INGRESSANTES', 'QUANT_CONCLUINTES']
    return retrieve(command, cols)
