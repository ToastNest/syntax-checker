#Construct variable declaration.
#   int a;    Optional : int a = 10
import ply.lex as lex

#variables

#reserved words (yet to add short int, long etc.)
reserved = { 'int' : 'INT', 'float' : 'FLOAT', 'double' : 'DOUBLE', 'char' : 'CHAR', 'bool' : 'BOOL'}

#Tokens
tokens = ['NUMBER','ID','SEMICOL','COMMA','POINTER','LSP','RSP'] + list(reserved.values())

#RegEx for token
t_SEMICOL = r'\;'
t_COMMA = r'\,'
t_POINTER = r'\*'
t_LSP = r'\['           #Left Sqaure Parenthesis
t_RSP = r'\]'           #Right Square Parenthesis

#RegEx rules
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

# data =  "int a,b;"

# lexer.input(data)

# while True:
#     tok = lexer.token()
#     if not tok:
#         break
#     print(tok)

print("Entering PARSING Stage")
##### BUILDING THE PARSER
import ply.yacc as yacc

def p_start(p):
    "start : dataType identifier SEMICOL"
    p[0] = p[1] +" "+ p[2] +" "+ p[3]

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

def p_identifier(p):
    '''identifier : identifier COMMA terminal
                  | terminal'''
    if(len(p)>2):
        p[0] = p[1] +","+ p[3]
    else:
        p[0] = p[1]
    
def p_terminal(p):
    '''terminal : pre ID post'''
    p[0] = p[1] + p[2] + p[3]

def p_pre(p):
    '''pre : POINTER
           | empty '''
    p[0] = p[1]

def p_post(p):
    '''post : LSP NUMBER RSP
            | empty '''
    if(len(p)>2):
        p[0] = p[1] + p[2] + p[3]
    else:
        p[0] = p[1]

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
       s = input('var dec > ')
       tokenChecker(s)
   except EOFError:
       break
   if not s: break
   result = parser.parse(s)
   print("Result:",result,"Type:",type(result))

print("EOP")