from tokenize import tokenize
from io import BytesIO


def logger(log):
    print(log)


def file_reader(file):
    return open(file, 'r').read()


def tokenizer(file):
    return tokenize(BytesIO(file_reader(file).encode('utf-8')).readline)


def lexer(file):
    lex = ' ['
    tokens = []
    for _, tval, _, _, _ in tokenizer(file):
        if(len(tval) != 0):
            if tval != "\n" and tval != "utf-8" and tval != "\t" and tval != '"':
                if tval == '[':
                    tokens.append(tval)

                    continue
                elif tval == ']':
                    tokens.append(tval)
                    lex += ("".join(tokens))
                    tokens = []

                else:
                    if tval.startswith('"'):
                        x = tval[1:-1]
                        logger(x)
                        lex += "'" + '"' + "'" + ',' + "'" + x + "'" + ',' + "'" + '"' + "'"
                    elif tval == ')' or tval == '(' or tval == '{' or tval == '}':
                        lex += "'" + tval + "'"
                    elif tval == '!=':
                        lex += "'" + tval + "'"
                    else:
                        lex += tval
                lex += ","

    lex = lex[:-1]
    lex += " ]"
    logger(lex)

    return lex
