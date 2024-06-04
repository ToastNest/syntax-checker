import ply.lex as lex

#reserved words
reserved = { 'for' : 'FOR',
            'int' : 'INT', 'float' : 'FLOAT', 'double' : 'DOUBLE', 'char' : 'CHAR', 'bool' : 'BOOL'}

#tokens 
tokens = ['LP','RP','LCP','RCP','EQUALS','SEMICOL','PLUS','MINUS','MUL', 'DIV','EXP',
          'GT','LT','GTE','LTE','EE','NE','NOT',
          'NUMBER','ID'] + list(reserved.values())

#RegEx for tokens
t_LP = r'\('    #Left Parenthesis
t_RP = r'\)'    #Right Parenthesis
t_LCP = r'\{'   #Left Curly
t_RCP = r'\}'   #Right Curly
t_EQUALS = r'\='    #Assignment Equals
t_SEMICOL = r'\;'   #Semicolon
t_PLUS = r'\+'      #Plus sign
t_MINUS = r'\-'     #Minus sign
t_MUL = r'\*'       #Multiplication sign
t_DIV = r'\/'       #Division sign
t_EXP = r'\^'       #Exponent sign
t_GT = r'\>'    #Greater than
t_LT = r'\<'    #Lesser than
t_GTE = r'\>\='   #Greater than Equal
t_LTE = r'\<\='   #Lesser than Equal
t_EE = r'\=\='    #Relational Equals
t_NE = r'\!\='    #Not Equal
t_NOT = r'\!'     #NOT

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

# data = '''for (int i = 0; i <5; i++) { asdf = 10 }'''

# lex.input(data)

# while True:
#     tok = lexer.token()
#     if not tok:
#         break
#     print(tok)

print("ENTERING PARSING STAGE")
####### BUILDING THE PARSER

import ply.yacc as yacc

def p_start(p):
    ''' start : FOR LP initialize SEMICOL condition SEMICOL update RP body'''
    p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6] + p[7] + p[8] + p[9]

def p_dataType(p):
    '''dataType : INT
                | FLOAT
                | CHAR
                | DOUBLE
                | BOOL'''
    p[0] = p[1]

def p_empty(p):
    'empty : '
    p[0] = ""

def p_initialize(p):
    '''initialize : dataType ID EQUALS rvalue
                  | ID EQUALS rvalue
                  | rvalue
                  | empty'''
    if(len(p)>4):
        p[0] = p[1] + p[2] + p[3] + p[4]
    elif (len(p)>3):
        p[0] = p[1] + p[2] + p[3]
    else:
        p[0] = p[1]

def p_rvalue(p):
    '''rvalue : ID
              | NUMBER'''
    p[0] = p[1]

def p_condition(p):
    '''condition : ID operator rvalue
                 | NOT rvalue
                 | empty'''
    if(len(p)==4):
        p[0] = p[1] + p[2] + p[3]
    elif(len(p)==3):
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1]

def p_operator(p):
    '''operator : GT
                | LT
                | GTE
                | LTE
                | EE
                | NE'''
    p[0] = p[1]


def p_update(p):
    '''update : ID PLUS PLUS
              | PLUS PLUS ID
              | ID MINUS MINUS
              | MINUS MINUS ID
              | empty'''
    if(len(p)>2):
        p[0] = p[1] + p[2] + p[3]
    else:
        p[0] = p[1]

def p_body(p):
    '''body : LCP statements RCP
            | SEMICOL LCP statements RCP'''
    if(len(p)>4):
        p[0] = p[1] + p[2] + p[3] + p[4]
    else:
        p[0] = p[1] + p[2] + p[3]

def p_statements(p):
    '''statements : statements terminal
                  | terminal'''
    if(len(p)>2):
        p[0] = p[1]+p[2]
    else:
        p[0] = p[1]

def p_terminal(p):
    '''terminal : ID 
                | dataType 
                | operator 
                | SEMICOL 
                | NUMBER
                | signs
                | empty'''
    p[0] = p[1]

def p_signs(p):
    '''signs : PLUS
             | MINUS 
             | MUL 
             | DIV 
             | EXP'''
    p[0] = p[1]

def p_error(p):
    print("Syntax Error in input!",p)
    

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