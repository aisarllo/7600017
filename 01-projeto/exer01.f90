program exer01 

        implicit none
        
        integer(4) :: referencia_simples = 1, bits_simples = 1
        real(4) :: a_simples = 1.0e0, aux_simples, divisor_simples = 2.0e0

        integer(8) :: referencia_dupla = 1, bits_dupla = 1
        real(8) :: a_dupla = 1.0d0, aux_dupla, divisor_dupla = 2.0d0

        integer(16) :: referencia_quad = 1, bits_quad = 1
        real(16) :: a_quad = 1.0_16, aux_quad, divisor_quad = 2.0_16

        ! precisao simples
        print *, 'PRECISAO SIMPLES'

        do while ((1 + a_simples) /= referencia_simples)
                aux_simples = a_simples
                print *, a_simples
                print *, (1 + a_simples)
                a_simples = a_simples / divisor_simples
                bits_simples = bits_simples + 1
        end do

        ! precisao dupla
        print *, 'PRECISAO DUPLA'

        do while ((1 + a_dupla) /= referencia_dupla)
                aux_dupla = a_dupla
                print *, a_dupla
                print *, (1 + a_dupla)
                a_dupla = a_dupla / divisor_dupla
                bits_dupla = bits_dupla + 1
        end do

        ! precisao quadrupla
        print *, 'PRECISAO QUADRUPLA'

        do while ((1 + a_quad) /= referencia_quad)
                aux_quad = a_quad
                print *, a_quad
                print *, (1 + a_quad)
                a_quad = a_quad / divisor_quad
                bits_quad = bits_quad + 1
        end do

        ! saidas
        write(*,*) (bits_simples - referencia_simples), aux_simples
        write(*,*) (bits_dupla - referencia_dupla), aux_dupla
        write(*,*) (bits_quad - referencia_quad), aux_quad

end program exer01 
