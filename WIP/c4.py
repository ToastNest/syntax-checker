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

# data = '''class classname { private: int a public: function()}'''

# lex.input(data)

# while True:
#     tok = lexer.token()
#     if not tok:
#         break
#     print(tok)


###### BUILDING THE PARSER

import ply.yacc as yacc

def p_start(p):
    '''start : CLASS ID LCP block RCP SEMICOL'''
    p[0] = p[1] + p[2] + p[3] + p[4] + p[5]

def p_block(p):
    '''block : block blockend
             | blockend'''
    if(len(p)>2):
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1]

def p_blockend(p):
    '''blockend : access COLON statements
                | empty'''
    if (len(p)>3):
        p[0] = p[1] + p[2] + p[3]
    else:
        p[0] = p[1]

def p_access(p):
    '''access : PRIVATE
              | PUBLIC
              | PROTECTED'''
    p[0] = p[1]

def p_statements(p):
    ''' statements : return ID LP parameters RP SEMICOL
                   | empty''' #variable declaration and function signatures only (can be empty too)
    if(len(p)>4):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6]
    else :
        p[0] = p[1]

def p_dataType(p):
    '''dataType : INT
                | FLOAT
                | CHAR
                | DOUBLE
                | BOOL'''
    p[0] = p[1]

def p_return(p):
    '''return : INT
                | FLOAT
                | CHAR
                | DOUBLE
                | BOOL
                | VOID'''
    p[0] = p[1]

def p_parameters(p):
    '''parameters : parameters COMMA param
                  | param'''
    if(len(p)>3):
        p[0] = p[1] + p[2] + p[3]
    else:
        p[0] = p[1]

def p_param(p):
    '''param : dataType ID
             | empty'''
    if(len(p)>2):
        p[0] = p[1] +" "+ p[2]
    else:
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
