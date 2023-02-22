program exer2 ! decaimento radioativo
      implicit none

        ! declaração das variáveis
        real(8) :: a, t_max = 10.0d0, t_min = 0.0d0, dt, lambda, tempo_medio, tempo_exato ! tempo: t de vida médio
        integer :: i, j, atomos_decaidos,  atomos_zero, iteracoes, n_atomos, ni_atomos

        ! entradas
        read(*,*) atomos_zero, dt, lambda
        open(2, file = "metropolis.dat", status = "replace")

        ! operações
        atomos_decaidos = 0
        ni_atomos = atomos_zero
        iteracoes = int((t_max - t_min) / dt)
        tempo_exato = (1.0d0 / lambda)

        ! (1) algoritmo de metrópolis
        do i = 1, iteracoes
                do j = 1, ni_atomos
                        call random_number(a) ! número aleatório

                        decaimento: if (a < (lambda * dt)) then
                                atomos_decaidos = (atomos_decaidos + 1)
                        end if decaimento
                end do

                tempo_medio = tempo_medio + (((i * dt) * atomos_decaidos) / atomos_zero)
                ni_atomos = (ni_atomos - atomos_decaidos)
                atomos_decaidos = 0

                write(2, *) (i * dt), ni_atomos
        end do

        tempo_medio = tempo_medio + ((10.0d0 * ni_atomos) / atomos_zero)
        print *, tempo_medio, tempo_exato

        close(2)
end program exer2
