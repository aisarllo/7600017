program exerA
      implicit none

        ! declaração das variáveis
        real(8) :: distancia, dt, periodo, x, y, xi, yi, t
        real(8) :: v_zero, x_zero, y_zero
        real(8), parameter :: GMS = (4 * (4 * atan(1.0d0)) ** 2)
        real(8), dimension(9) :: distancias, velocidades, tempos ! em relação ao sol
        integer :: i, j 

        ! entradas
        read *, distancia, v_zero, dt
        open(1, file = "trajA1_out.dat", status = "replace")
        open(2, file = "tabA1_out.dat", status = "replace")

        ! definição das constantes para cada planeta do sistema solar
        distancias(1) = 0.39d0   ! Mercúrio
        distancias(2) = 0.72d0   ! Vênus
        distancias(3) = 1.0d0    ! Terra
        distancias(4) = 1.52d0   ! Marte
        distancias(5) = 5.20d0   ! Júpiter
        distancias(6) = 9.24d0   ! Saturno
        distancias(7) = 19.19d0  ! Urano
        distancias(8) = 30.06d0  ! Netuno
        distancias(9) = 39.53d0  ! Plutão (planeta-anão)

        ! operações
        do i = 1, 9
                velocidades(i) = sqrt(GMS / distancias(i)) ! velocidades centrípetas de cada planeta
        end do

        ! condições iniciais:
        x_zero = distancia
        y_zero = 0.0d0
        x = distancia
        y = (y_zero + (v_zero * dt))
        t = 0.0d0
        write(1, *) t, x_zero, y_zero

        j = 0
        do while (j == 0)
                distancia = (sqrt((x ** 2) + (y ** 2)))

                ! modelo de verlet:
                yi = (2 * y - y_zero - (dt ** 2) * GMS * y / (distancia ** 3))
                xi = (2 * x - x_zero - (dt ** 2) * GMS * x / (distancia ** 3))

                if (yi > 0 .and. y < 0) then
                        j = j + 1
                end if

                ! reset das variáveis:
                y_zero = y
                x_zero = x
                x = xi
                y = yi
                t = (t + dt)

                ! saídas:
                write(1, *) t, xi, yi
        end do

        periodo = t

        ! construção da tabela "tab1_out.dat":
        do j = 1, 9
                i = 0
                x_zero = distancias(j)
                y_zero = 0.0d0
                y = (y_zero + velocidades(j) * dt)
                x = distancias(j)
                t = 0.0d0

                ! método de verlet:
                do while (i == 0)
                        distancia = (sqrt((x ** 2) + (y ** 2)))
                        yi = (2 * y - y_zero - (dt ** 2) * GMS * y / (distancia ** 3))
                        xi = (2 * x - x_zero - (dt ** 2) * GMS * x / (distancia ** 3))

                        if (yi > 0 .and. y < 0) then
                                i = i + 1
                        end if

                        ! reset das variáveis:
                        y_zero = y
                        x_zero = x
                        x = xi
                        y = yi
                        t = (t + dt)
                end do
                
                tempos(j) = ((t ** 2) / (distancias(j) ** 3))
        end do

        write(2, *) 'Mercúrio', velocidades(1), tempos(1)
        write(2, *) 'Vênus',    velocidades(2), tempos(2)
        write(2, *) 'Terra',    velocidades(3), tempos(3)
        write(2, *) 'Marte',    velocidades(4), tempos(4)
        write(2, *) 'Júpiter',  velocidades(5), tempos(5)
        write(2, *) 'Saturno',  velocidades(6), tempos(6)
        write(2, *) 'Urano',    velocidades(7), tempos(7)
        write(2, *) 'Netuno',   velocidades(8), tempos(8)
        write(2, *) 'Plutão',   velocidades(9), tempos(9)

        close(1)
        close(2)

        ! dados para fazer os gráficos:
        open(1, file = "trajA1_out.dat")
        open(3, file = "graf4.dat", status = "replace")

        dt = (periodo / 4.0d0)
        do while (dt < periodo)
                read(1, *) dt, x, y
                
                if (dt >= (3 * periodo / 4.0d0) .and. dt < periodo) then
                        write(3, *) x, y
                end if
        end do

        ! 1º quadrante: dt >= (periodo / 4.0d0) .and. dt < periodo
        ! 2º quadrante: dt >= 2 * (periodo / 4.0d0) .and. dt < periodo
        ! 3º quadrante: dt >= 3 * (periodo / 4.0d0) .and. dt < periodo
        ! 4º quadrante: dt <= (periodo / 4.0d0) .and. dt < periodo

        close(1)
        close(3)
end program
