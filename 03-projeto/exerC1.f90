program exerC1 ! método de euler; pêndulo (aproximação harmônica)
        implicit none

        ! declaração das variáveis
        real(8), parameter :: g = 10.0d0, pi = (4 * atan(1.0_8))
        real(8) :: l, m, T, deltaT  
        real(8) :: theta, theta_zero, w, w_zero = 0.d0, E, E_zero
        integer :: n, i ! n: número de iterações

        ! leitura das entradas + arquivo de saídas
        read *, theta_zero
        read *, l
        read *, m
        read *, deltaT
        read *, T
        open(1, file = "exerC1_out.dat", status = "replace")
        open(2, file = "energia1.dat", status = "replace")

        ! operações      
        n = int(T / deltaT)
        theta_zero = (pi / 180d0) * theta_zero
        E = ((m * (w_zero ** 2) * (l ** 2) / 2.0d0) + (m * l * (1 - cos(theta_zero)) * g))
        write(1, *) 0.0d0, theta_zero
        write(2, *) 0.0d0, E

        do i = 1, n
                angulo: if (theta_zero > (2 * pi)) then
                        theta_zero = (theta_zero - (2 * pi))
                else if (theta < ((-2) * pi)) then
                        theta_zero = (theta_zero + (2 * pi))
                end if angulo

                w = (w_zero - ((g * theta * deltaT) / l))
                theta = (theta_zero + (w_zero * deltaT))

                E = ((m * (w ** 2) * (l ** 2) / 2.0d0) + (m * l * (1 - cos(theta)) * g))

                w_zero = w
                theta_zero = theta

                write(1, *) (deltaT * i), theta
                write(2, *) (deltaT * i), E
        end do
end program exerC1
