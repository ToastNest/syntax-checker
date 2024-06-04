import ply.lex as lex

reserved = { 'switch' : 'SWITCH','case' : 'CASE', 'default' : 'DEFAULT'}

#tokens 
tokens = ['LP','RP','LCP','RCP','EQUALS','SEMICOL','COLON',
          'NUMBER','ID'] + list(reserved.values())

#RegEx
t_LP = r'\('    #Left Parenthesis
t_RP = r'\)'    #Right Parenthesis
t_LCP = r'\{'   #Left Curly
t_RCP = r'\}'   #Right Curly
t_SEMICOL = r'\;'   #Semicolon
t_COLON = r'\:'   #Semicolon


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

# data = '''switch(ch) { case 1: break; default:}'''

# lex.input(data)

# while True:
#     tok = lexer.token()
#     if not tok:
#         break
#     print(tok)

######### BUILDING PARSER

import ply.yacc as yacc

def p_start(p):
    '''start : SWITCH LP ID RP LCP statements RCP'''
    p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6] + p[7]

def p_statements(p):
    '''statements : statements terminal
                  | terminal 
                  | empty'''
    if(len(p)>2):
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1]


def p_terminal(p):
    '''terminal : CASE rvalue COLON anything'''
    p[0] = p[1] + p[2] + p[3] + p[4]

def p_rvalue(p):
    ''' rvalue : ID
                | NUMBER'''
    p[0] = p[1]

def p_anything(p):
    '''anything : empty'''
    p[0] = p[1]

def p_empty(p):
    'empty : '
    p[0] = ""

def p_error(p):
    print("Syntax Error in input",p)

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
       s = input('var dec > ')
       tokenChecker(s)
   except EOFError:
       break
   if not s: break
   result = parser.parse(s)
   print("Result:",result,"Type:",type(result))

print("EOP")
