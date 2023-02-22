program exer1 ! decaimento radioativo
        implicit none

        ! declaração das variáveis
        real(8) :: t_min, t_max, dt, lambda ! N: número de átomos
        integer :: i, j, N_zero, N, iteracoes

        ! leitura das entradas + arquivo de saída
        read *, N_zero, dt, lambda
        open(1, file = "decai_out.dat", status = "replace")

        ! operações
        t_min = 0.0d0
        t_max = 10.0d0
        iteracoes = int((t_max - t_min) / dt)
        write(1, *) (dt * 0), N_zero

        do i = 1, iteracoes
                N = int(N_zero - (lambda * N_zero * dt)) ! quantidade de átomos que decai

                N_zero = N
                write(1, *) (dt * i), N ! saídas
        end do

        close(1) ! fechamento do arquivo criado
end program exer1

