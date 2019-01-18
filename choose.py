#lang python

# Função simples de fatorial
def fatorial(n):
    if n < 1:
        return 1
    else:
        return n * fatorial(n - 1)

# Fórmula de n escolhe k implementada com fatoriais
def choose(n, k):
    return fatorial(n) / (fatorial(k) * fatorial(n - k))