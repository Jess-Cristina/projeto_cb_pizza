      *Divisão de identificação do programa
       identification division.
       program-id. "Projeto1-Pizza".
       author. "Jessica C. Del'agnolo".
       installation. "PC".
       date-written. 08/07/2020.
       date-compiled. 08/07/2020.



      *Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *-----Declaração dos recursos externos
       input-output section.
       file-control.
       i-o-control.

      *Declaração de variáveis
       data division.

      *----Variaveis de arquivos
       file section.


      *----Variaveis de trabalho
       working-storage section.

       01  relatorio  occurs  20.
           05 nome                                 pic x(15).
           05 filler                               pic x(03)
              value " - ".
           05 diametro                             pic 9(03).
           05 filler                               pic x(03)
              value " - ".
           05 preco                                pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 preco_cm2                            pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 diferenca_rel                        pic 9(03)v99.

       77 ind                                      pic 9(02).
       77 menu                                     pic 9(01).
       77 diametro_elev                            pic 9(03).
       77 area_pizza                               pic 9(10)v9(08).
       77 pi                                       pic 9(10)V9(08)
                                                   value 3,14159265.
       77 qtd                                      pic 9(02).
       77 controle                                 pic x(10).
       77 aux                                      pic 9(03)v99.
       77 aux2                                     pic x(15).
       77 aux3                                     pic 9(03).
       77 aux4                                     pic 9(03)v99.
       77 diferenca                                pic 9(03)v99.



      *----Variaveis para comunicação entre programas
       linkage section.


      *----Declaração de tela
       screen section.


      *Declaração do corpo do programa
       procedure division.


           perform inicializa.
           perform cadastro_pizza.
           perform calculos.
           perform ordena.
           perform calcula_diferenca.
           perform relatorio_.
           perform finaliza.

      * Inicilizacao de variaveis, abertura de arquivos
      * procedimentos que serao realizados apenas uma vez
       inicializa section.

           display "--- Calculo de Custo Beneficio das Pizzas ---"

           .
       inicializa-exit.
           exit.

      *-----------------------------------------------------------------

       cadastro_pizza section.

           move 0 to qtd
           move 1 to ind
           move 0 to menu

      *    Registro das pizzas.
           perform until menu = 2

               if ind > 20 then
                   display "Voce Atingiu o Limite de 20 Pizzas"
               else
                   display "Informe o Nome da Pizza "
                   accept nome(ind)

                   display "Informe o Diametro "
                   accept diametro(ind)

                   display "Informe o Preco "
                   accept preco(ind)

      *            Adicionar 1 para qtd de pizzas e 1 para indexador
                   add 1 to qtd
                   add 1 to ind
               end-if

      *        Menu para escolher se que cadastrar mais pizzas
               display " "
               display "Deseja Cadastrar Mais uma Pizza?"
               display "1 - Sim."
               display "2 - Nao."
               accept menu
               display erase

           end-perform


           .
       cadastro_pizza-exit.
           exit.

      *-----------------------------------------------------------------

       relatorio_ section.

      *    Exibição do relatório final.

           move 1 to ind

           perform qtd times
               display relatorio(ind)
               add 1 to ind
           end-perform

           .
       relatorio_-exit.
           exit.

      *-----------------------------------------------------------------

       calculos section.

           move 1 to ind

      *    Calcular área e obter o preco por cm² das pizzas
           perform qtd times

      *        Calculo de area da pizza
               compute area_pizza = pi * (diametro(ind) * diametro(ind))

      *        Calculo de preco por cm²
               divide preco(ind) by area_pizza giving preco_cm2(ind)

               add 1 to ind

           end-perform

           .
       calculos-exit.
           exit.

      *-----------------------------------------------------------------

       ordena section.

               move 0 to aux
               move "a" to aux2
               move 0 to aux3
               move 0 to aux4
               move 0 to preco_cm2(ind)

      *        Ordenar tabela de relatorio por custo beneficio
               move "continua" to controle

      *        Ordenar parada quando estiver na ordem correta
               perform until controle <> "continua"
                   move 1 to ind

                   move "n_continua" to controle
                   perform until ind = qtd
                       if preco_cm2(ind) > preco_cm2(ind + 1) then

      *                    Mover ind + 1 para variaveis auxiliares
                           move preco_cm2(ind + 1) to aux
                           move nome(ind + 1) to aux2
                           move diametro(ind + 1) to aux3
                           move preco(ind + 1) to aux4

      *                    Mover mover ind para ind + 2
                           move preco_cm2(ind) to preco_cm2(ind + 1)
                           move nome(ind) to nome(ind + 1)
                           move diametro(ind) to diametro(ind + 1)
                           move preco(ind) to preco(ind + 1)

      *                    Mover valor guardado na variavel auxiliar
      *                    para ind
                           move aux to preco_cm2(ind)
                           move aux2 to nome(ind)
                           move aux3 to diametro(ind)
                           move aux4 to preco(ind)

                           move "continua" to controle

                       end-if
                       add 1 to ind
                   end-perform
               end-perform
           .
       ordena-exit.
           exit.

      *-----------------------------------------------------------------

       calcula_diferenca section.

           move 1 to ind
           move 0 to diferenca_rel(1)

      *    Calcular a diferença entre o custo beneficio das pizzas
           perform varying ind from 1 by 1 until nome(ind) = space

               compute diferenca = preco_cm2(ind + 1) - preco_cm2(ind)

               compute diferenca_rel(ind + 1) = (diferenca * 100)
                       / preco_cm2(ind)

           end-perform

           .
       calcula_diferenca-exit.
           exit.

      *-----------------------------------------------------------------

       finaliza section.
           Stop run
           .
       finaliza-exit.
           exit.













