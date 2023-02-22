program exerA ! método de euler
        implicit none

        ! declaração das variáveis
        real(8), parameter :: m = 80.0d0, P = 400.0d0 ! m: massa; P: potência
        real(8) :: T, deltaT ! tempo total e sua variação em segundos
        real(8) :: v, v_zero
        integer :: i, n ! n: número máximo de iterações

        ! entrada + arquivo de saída
        read *, T, deltaT, v_zero
        open(1, file = 'velA_out.dat', status = 'replace')

        ! operações (método de euler)
        n = int(T / deltaT)
        write(1, *) (deltaT * 0), v_zero

        do i = 1, n
                v = (v_zero + ((P * deltaT) / (m * v_zero)))
                v_zero = v
                write(1, *) (deltaT * i), v
        end do

        close(1) ! fechamento do arquivo aberto
end program exerA
