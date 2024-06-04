import ply.lex as lex

reserved = {'private' : 'PRIVATE', 'public' :'PUBLIC', 'protected' : 'PROTECTED','class' : 'CLASS',
             'int' : 'INT', 'float' : 'FLOAT', 'double' : 'DOUBLE', 'char' : 'CHAR', 'bool' : 'BOOL','void':'VOID'}

tokens = ['LP','RP','LCP','RCP','EQUALS','SEMICOL','COLON','COMMA',
          'NUMBER','ID'] + list(reserved.values())

#RegEx
t_LP = r'\('    #Left Parenthesis
t_RP = r'\)'    #Right Parenthesis
t_LCP = r'\{'   #Left Curly
t_RCP = r'\}'   #Right Curly
t_SEMICOL = r'\;'   #Semicolon
t_COLON = r'\:'   #Semicolon
t_COMMA = r'\,'     #Comma

#RegEx Rules
def t_NUMBER(t):
    r'\d+'
    # t.value = (int)(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')    # Check for reserved word
    return t

t_ignore  = ' \t'

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

import ply.yacc as yacc

def p_start(p):
    '''start : CLASS ID COLON access ID LCP empty RCP'''
    p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6] + p[7] + p[8]

def p_access(p):
    '''access : PRIVATE
              | PUBLIC
              | PROTECTED'''
    p[0] = p[1]

def p_empty(p):
    '''empty : '''
    p[0] = ""

def p_error(p):
    print("Syntax error in input!")

parser = yacc.yacc()

def tokenChecker(data):
    lexer.input(data)

    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)


while True:
   try:
       s = input('inheritance > ')
       tokenChecker(s)
   except EOFError:
       break
   if not s: break
   result = parser.parse(s)
   if(result):
       print("Syntax Accepted")
       print("Result:",result,"Type:",type(result))
   else:
       print("Syntax Rejected")