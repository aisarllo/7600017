program exerC ! raízes de uma função
        implicit none

        ! declaração das variáveis
        real(8) :: f, derivada_f, x, xanterior ! x: auxiliares
        real(8), dimension(3) :: x1_buscadireta, x2_buscadireta, buscadireta ! método (1)
        real(8), dimension(3) :: x_newton, x0_secante, x1_secante, newton, secante ! métodos (1) e (2)
        real(8), dimension(10) :: saidas ! diagramação da tabela de saídas
        integer :: i, j, n ! n: número de iterações

        ! leitura da entrada
        read(*, *) n
        open(1, file = 'tabC_out.dat', status = 'replace') ! arquivo de saída

        ! definição dos valores iniciais de x para cada método (análise do gráfico da função)
        x1_buscadireta = (/-1.0d0, 0.0d0, 2.0d0/) ! f(x) > 0 
        x2_buscadireta = (/-2.0d0, 1.0d0, 1.0d0/) ! f(x) < 0

        x_newton = (/-1.5d0, 0.5d0, 1.5d0/) ! chutes método de newton-raphson

        x0_secante = (/-2.00d0, 0.00d0, 2.25d0/) ! chute de x-zero método da secante
        x1_secante = (/-1.99d0, 0.01d0, 2.24d0/) ! chute de x-um método da secante

        ! operações
        do i = 1, n
                saidas(1) = i

                ! (1) busca direta
                ! na busca direta, deve-se chutar dois valores de x de forma que f(x1) > 0 e f(x2) < 0
                ! haverá, então, uma raiz no intervalo, que será iterativamente diminuído
                do j = 1, 3
                        buscadireta(j) = ((x1_buscadireta(j) + x2_buscadireta(j)) / 2)

                        if (f(buscadireta(j)) > 0) then
                                x1_buscadireta(j) = buscadireta(j)
                        else
                                x2_buscadireta(j) = buscadireta(j)
                        end if

                        saidas(j + 1) = buscadireta(j)
                end do

                ! (2) método de newton-raphson
                do j = 1, 3
                        if (i == 1) then
                                saidas(j + 4) = x_newton(j)
                        else
                                x_newton(j) = (x_newton(j) - (f(x_newton(j)) / derivada_f(x_newton(j))))
                                newton(j) = x_newton(j)
                                saidas(j + 4) = newton(j)
                       end if
                end do

                ! (3) método da secante
                do j = 1, 3
                        if (i == 1) then
                                saidas(j + 7) = x0_secante(j)
                        else if (i == 2) then
                                saidas(j + 7) = x1_secante(j)
                        else
                                xanterior = x0_secante(j)
                                x = x1_secante(j)
                                x0_secante(j) = x

                                x1_secante(j) = (x - f(x) * ((x - xanterior) / (f(x) - f(xanterior))))

                                secante(j) = x1_secante(j)
                                saidas(j + 7) = secante(j)
                        end if
                end do

                ! impressão das saídas
                write(1, *) saidas
        end do

        close(1) ! fechamento do arquivo de saídas
end program exerC

! a título de comparação, as raízes da função estudada (x^3 - x^2 - 2x + 1) são aproximadamente:
! x1 = -1.24697960, x2 = 0.44504186 e x3 = 1.80193773

function f(x)
        implicit none
        real(8) :: f, x

        f = ((x ** 3) - (x ** 2) - (2 * x) + 1)
end function f

function derivada_f(x)
        implicit none
        real(8) :: derivada_f, x

        derivada_f = (3 * (x ** 2) - (2 * x) - 2)
end function derivada_f
