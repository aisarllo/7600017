program exer3 ! decaimento radioativo
      implicit none

        ! declaração das variáveis
        real(8) :: t_max, t_min = 0.0d0, dt, lambda
        integer :: i, iteracoes, atomos_zero, n_atomos, ni_atomos

        ! entradas
        open(2, file = "decai_in.dat", status = "old")
        read(2, *) t_max, atomos_zero, dt, lambda
        open(1, file = "decai_out.dat", status = "replace")
        write(1, *) 0.0d0, atomos_zero

        ! operações
        iteracoes = ((t_max - t_min) / dt)
        ni_atomos = atomos_zero
        do i = 1, iteracoes
                n_atomos = (ni_atomos * exp((- lambda) * (i * dt)))
                ni_atomos = n_atomos
                write(1, *) (i * dt), n_atomos
        end do

        close(1)
        close(2)
end program exer3
