# >------>--> Import's <--<-------<
import ply.lex as lex
import ply.yacc as yacc
import PySimpleGUI as sg
import emoji as e
import tkinter as tk
import matplotlib.pyplot as plt
from matplotlib.font_manager import FontProperties
import random as rng

# >------>--> Vars <--<-------<
vars = {}

erro_ = ""

# >------>--> Tokens <--<-------<
tokens = (
    'STRING',
    'EQUAL',
    'NUMBER',
    'STR',
    'ID',
    'INT',
    'RETURN',
    'COMENT',
    'PLUS',
    'MINUS',
    'WHILE',
    'LPAREN',
    'RPAREN',
    'EQUALS',
    'NOTEQUAL'
)

# >------>--> Regras dos Tokens <--<-------<
t_ID = r'[a-zA-Z_][a-zA-Z0-9_]*'

t_PLUS = r'\+'
t_MINUS = r'\-'

t_STR = r'\🔡'
t_INT = r'\🔢'

t_WHILE = r'\🔁'

t_LPAREN = r'\👉'
t_RPAREN = r'\👈'

t_NUMBER = r'\d+'
t_STRING = r'(\'[^\']*\'|\"[^\"]*\")'

t_EQUAL = r'='
t_EQUALS = r'=\?'
t_NOTEQUAL = r'\!='

t_RETURN = r'\↩'
t_COMENT = r'\📌(.+)'

# >------>--> Ignorar carateres em branco <--<-------<
t_ignore = ' \t\n'

# >------>--> Gramatica <--<-------<

# >------>--> Erros <--<-------<
def t_error(t):
    print("Caractere ilegal '%s'" % t.value[0])
    global erro_
    if t:
        erro_ = str("Unexpected '%s'" % t.value[0])
    else:
        erro_ = ""
    #p_error(str(t.value[0]))
    t.lexer.skip(1)

# >------>--> Programa <--<-------<
def p_program(p):
    '''
    program : statement
            | program statement
            | expression
            | program expression
            | while_statement
            | program while_statement
    '''
    #p[0] = tuple(p[1:])
    try:
        p[0] = p[len(p)-1]
    except:
        p[0] = "Nothing"

# >------>--> Var Int <--<-------<
def p_statement_int(p):
    'statement : INT ID EQUAL expression'
    try:
        vars[p[2]] = float(p[4])
    except:
        global erro_
        erro_ = "🆘 Cannot read '%s' like a 🔢" % p[4]

# >------>--> Var Str <--<-------<
def p_statement_str(p):
    'statement : STR ID EQUAL expression'
    try:
        vars[p[2]] = str(p[4])
    except:
        global erro_
        erro_ = "🆘 Cannot read '%s' like a 🔡" % p[4]

# >------>--> Change Var <--<-------<
def p_statement_var_change(p):
    'statement : ID EQUAL expression'
    try:
        vars[p[1]] = str(p[3])
    except:
        global erro_
        erro_ = "🆘 Cannot read '%s'" % p[1]

# >------>--> Terms (number first) <--<-------<
def p_term_number(p):
    '''term : expression
            | statement
            | INT
            | STR
    '''
    try:
        p[0] = vars(p[1])
    except:
        p[0] = p[1]

# >------>--> Plus <--<-------<
def p_statement_plus(p):
    '''expression : expression PLUS term
                  | expression PLUS expression
    '''
    try:
        p[0] = p[1] + p[3]
    except:
        p[0] = str(p[1]) + str(p[3])

# >------>--> Minus <--<-------<
def p_statement_minus(p):
    '''expression : expression MINUS term
                  | expression MINUS expression
    '''
    try:
        p[0] = p[1] + p[3]
    except:
        p[0] = str(p[1]) + str(p[3])

# >------>--> Paren () <--<-------<
def p_expression_paren(p):
    '''expression : LPAREN expression RPAREN
                  | LPAREN statement RPAREN
    '''
    p[0] = p[2]

# >------>--> Paren while () <--<-------<
def p_expression_paren_while(p):
    '''expression_while : LPAREN expression RPAREN
                        | LPAREN statement RPAREN
                        | LPAREN program RPAREN
    '''
    p[0] = p[2]
    print(p[0])

def verificar_verdadeiro(a, b):
    return a == b

# >------>--> While <--<-------<
def p_while_statement(p):
    '''while_statement : WHILE expression EQUALS expression
                       | WHILE expression EQUALS expression expression_while
                       | WHILE expression NOTEQUAL expression
                       | WHILE expression NOTEQUAL expression expression_while
    '''
    if p[3] == "==":
        if len(p) == 5:
            if (p[2] == p[4]):
                p[0] = str(True)
            else:
                p[0] = str(False)
        
        elif len(p) == 6:
            while True:
                if verificar_verdadeiro(p[2], p[4]):
                    p_expression_paren_while(p[5])
                else:
                    break

            p[0] = p[5]

    elif p[3] == "!=":
        if len(p) == 5:
            if (p[2] != p[4]):
                p[0] = str(True)
            else:
                p[0] = str(False)
        
        elif len(p) == 6:
            while True: 
                if verificar_verdadeiro(p[2], p[4]) == False:
                    p_expression_paren_while(p)
                    #print(p)
                else:
                    break
            p[0] = p[5]

'''🔢 c = 0
🔁 c != 5 👉 
c = c + 1
👈
↩ c '''

# >------>--> Identificador <--<-------<
def p_expression_id(p):
    '''
    expression : ID
    '''
    p[0] = vars.get(p[1], 0)

# >------>--> Inteiro // Float <--<-------<
def p_expression_num(p):
    'expression : NUMBER'
    p[0] = int(p[1])

# >------>--> String <--<-------<
def p_expression_str(p):
    'expression : STRING'
    arra = []
    str_p = ""
    for i in p[1]:
        arra.append(i)
    for j in range(0, len(arra) - 1):
        if j != len(arra) - 1 and j != 0:
            str_p = str_p + str(arra[j])
    p[0] = str_p

# >------>--> Return <--<-------<
def p_statement_return(p):
    '''
    statement : RETURN expression
              | RETURN statement
    '''
    try:
        p[0] = '👉 ' + str(vars(p[1]))
    except:
        p[0] = '👉 ' + str(p[2])

# >------>--> Coment <--<-------<
def p_statement_coment(p):
    '''
    statement : COMENT
    '''
    pass

# >------>--> Juntando Tokens e Gramática <--<-------<

lexer = lex.lex()
parser = yacc.yacc()

# >------>--> Compilação <--<-------<
entrada = ''
resultado = parser.parse(entrada)
font_path = "FC.ttf"
font = FontProperties(fname = font_path)

# >------>--> Compilador <--<-------<
try:
    font_emoji = ('Fira Code', 15)
except:
    font_emoji = (font, 15)
last_event = ""
code = "↩ 'Hello World'"

#ic = r"icons\ico'%s'.ico" % (rng.randint(0, 9))
choise = (rng.randint(0, 9))
match choise:
    case 0:
        ic = r"icons\ico0.ico"
        titulo = "Don't report Bugs"
    case 1:
        ic = r"icons\ico1.ico"
        titulo = "A trash dictionary"
    case 2:
        ic = r"icons\ico2.ico"
        titulo = "Texting Emoji Supremacy Translator (TEST)"
    case 3:
        ic = r"icons\ico3.ico"
        titulo = "A PERSON! OMG"
    case 4:
        ic = r"icons\ico4.ico"
        titulo = "Wi-fi"
    case 5:
        ic = r"icons\ico5.ico"
        titulo = "More file?"
    case 6:
        ic = r"icons\ico6.ico"
        titulo = "A trash editor! WRITE IN THEN ALL"
    case 7:
        ic = r"icons\ico7.ico"
        titulo = "Loop of noobs"
    case 8:
        ic = r"icons\ico8.ico"
        titulo = "That got killed my eyes"
    case 9:
        ic = r"icons\ico9.ico"
        titulo = "A pc to a man"

def new_theme(theme_new):
    if theme_new:
        sg.theme(theme_new)
    layout = [
            [sg.Text('Texting Emoji Supremacy Translator (TEST)', font="Arial 20 bold")],
            [sg.Text(str('Theme selected: ' + theme_new), font="Arial 10"), sg.Combo(values = sg.theme_list(), size =(25, 10), key ='LIST', enable_events = True)],
            [sg.Text('Code here:', font= font_emoji)],
            [sg.Multiline(e.emojize(code), size=(50, 10), font= font_emoji, key="INPUT", enable_events=True)],
            [sg.Button(e.emojize('🔡 \n :str:'), font= font_emoji), sg.Button('🔢 \n :num:', font= font_emoji), sg.Button('↩ \n :return:', font= font_emoji), sg.Button('📌 \n :coment:', font= font_emoji), sg.Button('👉 \n :(:', font= font_emoji), sg.Button('👈 \n :):', font= font_emoji), sg.Button('🔁 \n :while:', font= font_emoji)],
            [sg.Text(e.emojize('>...<'), font=font_emoji, text_color="#00ff55", key="OUTPUT"), sg.Text(e.emojize(""), font=font_emoji, text_color="#ff0044", key="OUTPUT-2")],
            [sg.Button('Execute', font= font_emoji), sg.Button('Close', font= font_emoji)] 
        ]
    new_window = sg.Window(titulo, layout, icon=ic, return_keyboard_events=True)
    return new_window

window = new_theme('DarkPurple4')
while True:
    event, values = window.read()
    if event is not None:
        code = values['INPUT']
    if event == " ":
        if last_event == ":":
            values['INPUT'] = values['INPUT'].replace(":str:", "🔡")
            values['INPUT'] = values['INPUT'].replace(":num:", "🔢")
            values['INPUT'] = values['INPUT'].replace(":return:", "↩")
            values['INPUT'] = values['INPUT'].replace(":coment:", "📌")
            values['INPUT'] = values['INPUT'].replace(":(:", "👉")
            values['INPUT'] = values['INPUT'].replace(":):", "👈")
            values['INPUT'] = values['INPUT'].replace(":while:", "🔁")
            window["INPUT"].update(values['INPUT'] + " ")
        last_event = event
    if event == ":":
        last_event = event
    if event == sg.WIN_CLOSED or event == 'Close':
        break
    elif event == 'Execute':
        resultado = parser.parse(values['INPUT'])
        window["OUTPUT"].update(e.emojize(">" + str(resultado) + "<"), font=font_emoji, text_color="#00ff55")
        window["OUTPUT-2"].update(e.emojize(str(erro_)), font=font_emoji, text_color="#ff0044")
        if erro_ != "":
            erro_ = ""
    elif event == '🔡 \n :str:':
        window["INPUT"].print('🔡')
    elif event == '🔢 \n :num:':
        window["INPUT"].print('🔢')
    elif event == '↩ \n :return:':
        window["INPUT"].print('↩')
    elif event == '📌 \n :coment:':
        window["INPUT"].print('📌')
    elif event == '👉 \n :(:':
        window["INPUT"].print('👉')
    elif event == '👈 \n :):':
        window["INPUT"].print('👈')
    elif event == '🔁 \n :while:':
        window["INPUT"].print('🔁')
    elif event == 'LIST':
        window.close()
        window = new_theme(values['LIST'])

window.close()

#👨‍💻📝✅❌↩🔢🔡🆘👉👈🤛🤜0️⃣1️⃣2️⃣3️⃣4️⃣5️⃣6️⃣7️⃣8️⃣9️⃣🔟⚠❓❗💥📁📂🗂📝🗃⏱⏰⌚📌📍🗑✏📷🖨