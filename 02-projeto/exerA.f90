program exerA ! diferenciação numérica
        implicit none

        ! declaração das variáveis
        real(8) :: f, h, derivada1, derivada2, derivada3 ! "resultados exatos"/analíticos
        real(8), dimension(6) :: erros, derivadas ! calculados numericamente
        real(8) :: fmenos2, fmenos1, f0, f1, f2
        real(8), parameter :: x = (1.0d0 / 3.0d0)
        real(8), allocatable, dimension(:) :: vetor_h
        integer(8) :: i, n ! n: número de valores de h

        ! leitura do arquivo de entrada
        open(1, file = 'tabA_in.dat', status = 'old')
        open(2, file = 'tabA_out.dat', status = 'replace') ! arquivo de saída
        read(1, *) n
        allocate(vetor_h(n)) ! alocação do vetor h
        read(1, *) vetor_h
        close(1)

        ! operações
        derivada1 = (1./2) * exp(4 * x) * (8 * cos(x / 2) - sin(x / 2))
        derivada2 = (1./4) * exp(4 * x) * (63 * cos(x / 2) - 16 * sin(x / 2))
        derivada3 = (1./8) * exp(4 * x) * (488 * cos(x / 2) - 191 * sin(x / 2))

        do i = 1, n
                ! reset das variáveis
                derivadas = (/0, 0, 0, 0, 0, 0/)
                erros = (/0, 0, 0, 0, 0, 0/)
                h = vetor_h(i)

                ! definição dos valores a serem utilizados
                fmenos2 = f(h, (-2))
                fmenos1 = f(h, (-1))
                f0 = f(h, 0)
                f1 = f(h, 1)
                f2 = f(h, 2)

                ! (1) derivada simétrica 3 pontos
                derivadas(1) = (f1 - fmenos1) / (2 * h)
                erros(1) = abs(derivadas(1) - derivada1)

                ! (2) derivada para frente 2 pontos
                derivadas(2) = (f1 - f0) / h
                erros(2) = abs(derivadas(2) - derivada1)

                ! (3) derivada para trás 2 pontos
                derivadas(3) = (f0 - fmenos1) / h
                erros(3) = abs(derivadas(3) - derivada1)

                ! (4) derivada segunda simétrica 3 pontos
                derivadas(4) = (f1 - (2 * f0) + fmenos1) / (h ** 2)
                erros(4) = abs(derivadas(4) - derivada2)

                ! (5) derivada segunda simétrica 5 pontos
                derivadas(5) = ((- f2) + (16 * f1) - (30 * f0) + &
                               (16 * fmenos1) - fmenos2) / (12 * (h ** 2))
                erros(5) = abs(derivadas(5) - derivada2)

                ! (6) derivada terceira anti-simétrica 5 pontos
                derivadas(6) = (f2 - (2 * f1) + (2 * fmenos1) - fmenos2) / (2 * (h ** 3))
                erros(6) = abs(derivadas(6) - derivada3)

                ! impressão das saídas
                write(2, *) h, erros
        end do

        close(2) ! fechamento do arquivo de saída
        deallocate(vetor_h) ! liberação da memória alocada

        ! análise dos resultados obtidos
        print *, 'A partir da comparação dos valores das derivadas calculados analitica e numericamente, ', &
                 'pode-se perceber que cada método utilizado é otimizado para um valor específico de h. ', &
                 'Logo, o erro (ou desvio) deve ser analisado e escolhido caso a caso. ', &
                 'Isso posto, a ordem de grandeza de h com a qual há menor erro em cada operação é: '
        print *, '(1). Derivada primeira simétrica com 3 pontos:   10^(-7);'
        print *, '(2). Derivada primeira para frente com 2 pontos: 10^(-8);'
        print *, '(3). Derivada primeira para trás com 2 pontos:   10^(-8);'
        print *, '(4). Derivada segunda simétrica com 3 pontos:    10^(-5);'
        print *, '(5). Derivada segunda simétrica com 5 pontos:    10^(-4);'
        print *, '(6). Derivada terceira simétrica com 5 pontos:   10^(-4).'

end program exerA

function f(h, n)
        implicit none
        real(8) :: f, h, x
        real(8), parameter :: x_inicial = (1.0d0 / 3.0d0)
        integer :: n

        x = (x_inicial + (n * h))
        f = exp(4.0d0 * x) * cos(x / 2.0d0)
end function f
