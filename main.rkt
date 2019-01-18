#lang racket

(require "choose.py")
(require racket/gui/base)

; Calcula n escolhe k e mostra na tela
(define (calc-choose-and-show)
  (define n-elementos ; define a quantidade total de elementos
    (string->number   ; converte o texto recebido para um número
     (get-text-from-user "Função de escolha"
                         "Digite a quantidade TOTAL de elementos ")))
  (define qtd-escolhida  ; define o tamanho do subgrupo escolhido
    (string->number      ; converte o texto recebido para um número
     (get-text-from-user "Função de escolha"
                         "Digite a quantidade de elementos escolhidos por vez ")))
  (if (= 1 ; Faz um teste de igualdade com o número do botão apertado na interface gráfica
         ; Cria uma interface simples
         (message-box/custom "Resultado da função de escolha" ; Título da janela
                             (format " ~a escolhe ~a = ~a "   ; Mensagem a ser mostrada
                                     n-elementos              ; (format é similar ao formatador do python)
                                     qtd-escolhida
                                     (:choose n-elementos qtd-escolhida) ; usa a função feita em "Python"
                                     )
                             "Calcular outra combinação"      ; Mensagem do primeiro botão
                             "Sair"                           ; Mensagem do segundo botão
                             #f                               ; O terceiro botão não existe
                             #f                               ; Possui janela pai? Não.
                             '(no-default)                    ; Não usa nenhum estilo específico
                             2))                              ; Qual código retornar se a janela for fechada.
      (calc-choose-and-show) ; Repete tudo se o primeiro botão for apertado
      #f                     ; Não faz nada se o segundo botão for apertado
      )
  )

(calc-choose-and-show)