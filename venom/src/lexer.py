import sys
from venom.utils.utils import lexer

if __name__ == "__main__":

    file = sys.argv[1]
    tokens = lexer(file)
