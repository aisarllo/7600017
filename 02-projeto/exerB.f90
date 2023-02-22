program exerB
        implicit none

        ! declaração das variáveis
        real(8) :: f, h, integral_analitica
        real(8), parameter :: x_inicial = 0, x_final = 1 ! limites de integração
        real(8), dimension(3) :: x1 ! usado para os métodos (1) e (2)
        real(8), dimension(5) :: x2 ! usado para o método (3)
        real(8), dimension(3) :: erros, integrais ! calculados numericamente
        real(8), allocatable, dimension(:) :: vetor_h
        integer(8), allocatable, dimension(:) :: valores_N
        integer(8) :: i, j, n ! n: numero de valores_N

        ! leitura do arquivo de entrada
        open(1, file = 'tabB_in.dat', status = 'old')
        open(2, file = 'tabB_out.dat', status = 'replace') ! arquivo de saída
        read(1, *) n
        allocate(valores_N(n), vetor_h(n)) ! alocação dos vetores com N e h
        read (1, *) (valores_N(i), i = 1, n)
        close(1)
        
        ! cálculo dos valores de h
        do i = 1, n
                vetor_h(i) = (x_final - x_inicial) / valores_N(i)
        end do

        ! operações
        integral_analitica = (cos(x_final / 2.0d0) - (1.0d0 / 3.0d0) * cos(3.0d0 * x_final / 2.0d0)) - &
                             (cos(x_inicial / 2.0d0) - (1.0d0 / 3.0d0) * cos(3.0d0 * x_inicial / 2.0d0))

        do i = 1, n
                ! reset das variáveis
                erros = (/0, 0, 0/)
                integrais = (/0, 0, 0/)
                h = vetor_h(i)

                do j = 1, valores_N(i), 2
                        x1(1) = ((j - 1) * h) ! para calcular f(-1)
                        x1(2) = (j * h) ! para calcular f(0)
                        x1(3) = ((j + 1d0) * h) ! para calcular f(1)

                        ! (1) regra do trapézio
                        integrais(1) = integrais(1) + ((h / 2.0d0) * (f(x1(3)) + (2d0 * f(x1(2)) + f(x1(1)))))

                        ! (2) regra de simpson
                        integrais(2) = integrais(2) + ((h / 3.0d0) * (f(x1(3)) + (4d0 * f(x1(2))) + f(x1(1))))
                end do

                erros(1) = abs(integrais(1) - integral_analitica)
                erros(2) = abs(integrais(2) - integral_analitica)

                ! (3) regra de bode
                do j = 1, valores_N(i), 4
                        x2(1) = ((j - 1d0) * h) ! para calcular f(0)
                        x2(2) = (j * h) ! para calcular f(1)
                        x2(3) = ((j + 1d0) * h) ! para calcular f(2)
                        x2(4) = ((j + 2d0) * h) ! para calcular f(3)
                        x2(5) = ((j + 3d0) * h) ! para calcular f(4)

                        integrais(3) = integrais(3) + ((2.0d0 * h / 45.0d0) * ((7d0 * f(x2(1))) + (32d0 * f(x2(2))) + &
                                                      (12d0 * f(x2(3))) + (32d0 * f(x2(4))) + (7d0 * f(x2(5)))))
                end do

                erros(3) = abs(integrais(3) - integral_analitica)

                ! impressão das saídas
                write(2, *) valores_N(i), h, erros 
        end do

        ! fechamento do aquivo e liberação da memória alocada
        close(2)
        deallocate(valores_N, vetor_h)

        ! análise dos resultados obtidos
        print *, 'A partir da análise dos resultados obtidos e expostos na tabela, fica evidente que diferentes ', &
                 'valores de N originam desvios com diferentes ordens de grandeza para cada método utilizado no ', &
                 'cálculo da integral. Tendo isso em mente, o valor de N que otimiza a operação para cada método é: '
        print *, '(1). Método do Trapézio: N = 4096;'
        print *, '(2). Método de Simpson:  N = 4096;'
        print *, '(3). Método de Bode:     N = 1024 ou N = 2048.'
        print *, 'Para o método de Bode, o erro encontrado para os dois valores de N indicados têm a mesma ordem ', &
                 'de grandeza, de 10^(-17); enquanto para o método do Trapézio o menor erro tem ordem 10^(-9), e, ', &
                 'de Simpson, 10^(-17). Com essas informações, tem-se autonomia para escolher o método mais ', &
                 'adequado para quaisquer operações de interesse.'
end program exerB

function f(x)
        implicit none
        real(8) :: f, x

        f = sin(x / 2.0d0) * cos(x)
end function f
