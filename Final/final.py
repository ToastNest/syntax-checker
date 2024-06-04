import ply.lex as lex

#reserved words
reserved = {'void' : 'VOID', 'int' : 'INT', 'float' : 'FLOAT', 'double' : 'DOUBLE', 'char' : 'CHAR', 'bool' : 'BOOL','auto':'AUTO',
            'public' : 'PUBLIC', 'private' : 'PRIVATE', 'protected' : 'PROTECTED',
            'for' : 'FOR' , 'switch' : 'SWITCH', 'case' : 'CASE', 'default' : 'DEFAULT','class' : 'CLASS'}
#tokens 
tokens = ['LP','RP','LCP','RCP','LSP','RSP',
          'GT','LT','GTE','LTE','EE','NE','NOT',
          'PLUS','MINUS', 'DIV','EXP',
          'EQUALS','SEMICOL','COLON','SCOPEOP','COMMA','POINTER',
          'NUMBER','ID'] + list(reserved.values())

#RegEx 
t_LP = r'\('    #Left Parenthesis
t_RP = r'\)'    #Right Parenthesis
t_LCP = r'\{'   #Left Curly
t_RCP = r'\}'   #Right Curly
t_LSP = r'\['           #Left Sqaure Parenthesis
t_RSP = r'\]'           #Right Square Parenthesis

t_GT = r'\>'    #Greater than
t_LT = r'\<'    #Lesser than
t_GTE = r'\>\='   #Greater than Equal
t_LTE = r'\<\='   #Lesser than Equal
t_EE = r'\=\='    #Relational Equals
t_NE = r'\!\='    #Not Equal
t_NOT = r'\!'     #NOT

t_PLUS = r'\+'      #Plus sign
t_MINUS = r'\-'     #Minus sign
t_DIV = r'\/'       #Division sign
t_EXP = r'\^'       #Exponent sign

t_EQUALS = r'\='    #Assignment Equals
t_SEMICOL = r'\;'   #Semicolon
t_COLON = r'\:'   #Semicolon
t_SCOPEOP = r'\:\:' #Scope Resolution Operator
t_COMMA = r'\,'     #Comma
t_POINTER = r'\*'   #Pointer Dereference symbol

#RegEx Rules
def t_NUMBER(t):
    r'\d+'
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

### BUILDING THE PARSER
import ply.yacc as yacc

#CFG Productions
def p_start(p):
    '''start : dataType identifier SEMICOL
             | FOR LP initialize SEMICOL condition SEMICOL update RP body
             | SWITCH LP ID RP LCP caseBlocks RCP
             | CLASS ID LCP classBlock RCP SEMICOL
             | return ID SCOPEOP ID LP parameters RP optional LCP statements RCP
             | CLASS ID COLON accesstype ID LCP statements RCP'''
    if(len(p) > 10):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6] + p[7] + p[8] + p[9] + p[10]
    elif(len(p)>9):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6] + p[7] + p[8] + p[9]
    elif(len(p)>8):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6] + p[7] + p[8]
    elif(len(p) > 7):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6] + p[7]
    elif(len(p)>5):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6]
    else:
        p[0] = p[1] + p[2] + p[3]


#COMMON TO CONSTRUCTS
def p_rvalue(p):
    '''rvalue : ID
              | NUMBER'''
    p[0] = p[1]

def p_empty(p):
    'empty : '
    p[0] = ""

def p_dataType(p):
    '''dataType : INT
                | FLOAT
                | CHAR
                | DOUBLE
                | BOOL
                | AUTO'''
    p[0] = p[1]

def p_return(p):
    '''return : INT
              | FLOAT
              | CHAR
              | DOUBLE
              | BOOL
              | VOID
              | AUTO'''
    p[0] = p[1]

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
                | empty'''
    p[0] = p[1]

#Variable Declaration
def p_identifier(p):
    '''identifier : identifier COMMA ident
                  | ident'''
    if(len(p)>2):
        p[0] = p[1] +","+ p[3]
    else:
        p[0] = p[1]
    
def p_ident(p):
    '''ident : pre ID post'''
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

#For Loops
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

#Switch case
def p_caseBlocks(p):
    '''caseBlocks : caseBlocks caseEnd
                  | caseEnd'''
    if(len(p)>2):
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1]


def p_caseEnd(p):
    '''caseEnd : CASE rvalue COLON empty
               | empty'''
    if(len(p)>4):
        p[0] = p[1] + p[2] + p[3] + p[4]
    else:
        p[0] = p[1]

#Class definition
def p_classBlock(p):
    '''classBlock : classBlock classEnd
             | classEnd'''
    if(len(p)>2):
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1]

def p_classEnd(p):
    '''classEnd : access COLON classStatements
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

def p_classStatements(p):
    ''' classStatements : return ID LP parameters RP SEMICOL
                        | dataType identifier SEMICOL
                        | empty''' #variable declaration and function signatures only (can be empty too)
    if(len(p)>4):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6]
    elif(len(p)>3):
        p[0] = p[1] + p[2] + p[3]
    else :
        p[0] = p[1]

#Constructors
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


def p_optional(p):
    '''optional : COLON initList
                | empty '''
    if(len(p)>2):
        p[0] = p[1] + p[2]
    else:
        p[0]=p[1]

def p_initList(p):
    '''initList : initList COMMA paraTerminal
                | paraTerminal'''
    
    if(len(p)>3):
        p[0] = p[1] + p[2] + p[3]
    else:
        p[0] = p[1]

def p_paraTerminal(p):
    '''paraTerminal : ID LP ID RP'''
    p[0] = p[1] + p[2] + p[3] + p[4]

#Inheritance
def p_accesstype(p):
    '''accesstype : PRIVATE
                  | PUBLIC
                  | PROTECTED
                  | empty'''
    p[0] = p[1]

def p_error(p):
    print("Syntax Error in input")

#PARSING
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
        s = input('construct > ')
        tokenChecker(s)
    except EOFError:
        break
    if not s: break
    result = parser.parse(s)
    if(result):
        print("\nSyntax Accepted")
        print("Result:",result,"Type:",type(result),"\n")
    else:
        print("\nSyntax Rejected\n")


print("EOP")
