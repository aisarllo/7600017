program exerB ! método de euler (agora com força resistiva do ar)
        implicit none
        
        ! declaração das variáveis
        real(8), parameter :: m = 80.0d0, P = 400.0d0, d = 1.2d0, C = 0.5d0 ! d: massa específica; C: coeficiente de arrasto
        real(8) :: T, deltaT, T_terminal = 0.0d0, espaco = 0.0d0, A ! A: área de choque
        real(8) :: v, v_zero, v_media
        integer :: i, n ! n: número máximo de iterações

        ! entrada + arquivo de saída
        read *, T, deltaT, v_zero, A
        open(2, file = 'velB_out.dat')

        ! operações (método de euler)
        n = int(T / deltaT)
        write(2, *) (deltaT * 0), v_zero ! saídas

        do i = 1, n
                espaco = (espaco + (v_zero * deltaT)) ! espaço percorrido durante o movimento

                v = (v_zero + ((P * deltaT) / (m * v_zero)) - ((C * d * A * (v_zero ** 2) * deltaT) / m))      

                ! a velocidade terminal é atingida para as seguintes condições:
                if ((v == v_zero) .and. (T_terminal == 0.0d0)) then
                        T_terminal = ((i - 1) * deltaT)
                end if

                v_zero = v
                write(2, *) (deltaT * i), v ! saídas
        end do

        v_media = (espaco / T) ! cálculo da velocidade média

        close(2) ! fechamento do arquivo aberto

        ! respostas das perguntas levantadas:
        ! 01)
        print *, 'Com base na análise dos dados obtidos, pode-se responder as questões levantadas. ', &
                 'Para o que ciclista atinja a maior velocidade possível, é necessário que ele diminua ', &
                 'a área sobre a qual age a força resistiva do ar; isto é, a área de choque, fato que ', &
                 'reduz o termo da força de arrasto. Isso é o que ', &
                 'o faz curvar-se durante as corridas, ação que diminui a área do conjunto ciclista-bicicleta. ', &
                 'Por motivo análogo, os ciclistas, durante longos percursos, correm em grupo. Nessa situação, ', &
                 'a presença dos demais competidores, especialmente do competidor à frente, diminui o efeito da ', &
                 'resistência do ar sobre o corredor em questão. Novamente, isso torna possível que ele alcance ', &
                 'maiores velocidades. ', &
                 'Essa é precisamente a razão pela qual um ciclista possui mais vantagem ao andar imediatamente ', &
                 'atrás de seu adversário do que ao ultrapassá-lo. No processo de ultrapassagem, está sujeito a maior ', &
                 'força resistiva; ou seja, é uma ação custosa energeticamente, que causa a diminuição de sua velocidade. ', &
                 'Quando atrás de outro competidor, a presença deste reduz a força de arrasto sobre ele próprio, ', &
                 'possibilitando o alcance de velocidade maior. ', &
                 'Vale lembrar que, na situação analisada, que possui ação da força resistiva de arrasto, existe ', &
                 'um limite físico para o qual a velocidade do conjunto ciclista-bicicleta pode chegar, que será descrita ', &
                 'em resposta abaixo.' 

        ! 02)
        print *, 'O espaço total percorrido pelo ciclista é de: ', espaco
        ! 03)
        print *, 'A velocidade final do ciclista é de: ', v
        ! 04)
        print *, 'A velocidade final ou terminal é alcançada no tempo: ', T_terminal
        ! 05)
        print *, 'A velocidade média do ciclista no tempo T é de: ', v_media

end program exerB
