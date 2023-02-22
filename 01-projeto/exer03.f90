program exer03

        implicit none

        ! declaracao das variaveis
        integer(8) :: a = 2, b, i, primo
        real(8) :: maximo
        logical, allocatable, dimension(:) :: matriz
        
        ! leitura da entrada e criacao de arquivo para saida
        read *, maximo        
        open(1, file = 'primos_out.dat', status = 'replace')

        ! contabilizacao dos primos (crivo de eratostenes)
        allocate(matriz(int(maximo)))
        do i = 1, int(maximo)
                matriz(i) = .true.
        end do

        do while (a < sqrt(maximo))
                if (matriz(a)) then
                        b = (a ** 2)
                        do while (b <= maximo)
                                matriz(b) = .false.
                                b = (b + a)
                        end do
                end if
                a = (a + 1)
        end do

        do primo = 2, int(maximo)
                if (matriz(primo) .eqv. .true.) then
                        write(1, *) primo
                end if
        end do

end program exer03
