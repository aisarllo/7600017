program exer04

        implicit none

        ! declaracao das variaveis
        real, dimension(3) :: c1, c2, c3, c4, v1, v2, v3, v4, v5, v6 ! c: coordenadas; v: vetores
        real :: volume, soma_areas, a1, a2, a3, a4 ! a: areas
        real, dimension(4) :: areas
        real :: area_triangulo, volume_tetraedro
        integer :: maximo = 3, i

        ! leitura das coordenadas dos vertices
        open(1, file = 'vet_in.dat', status = 'old')
        read(1, *) (c1(i), i = 1, maximo)
        read(1, *) (c2(i), i = 1, maximo)
        read(1, *) (c3(i), i = 1, maximo)
        read(1, *) (c4(i), i = 1, maximo)
        close(1)

        ! criacao dos vetores
        v1 = (c2 - c1)
        v2 = (c3 - c1) ! face 1: c1, v1, v2
        v3 = (c2 - c3)
        v4 = (c4 - c3) ! face 2: c2, v3, v4
        v5 = (c4 - c1) ! face 3: c1, v1, v5
        v6 = (c1 - c3) ! face 4: c3, v4, v6
       
        ! area das faces do tetraedro
        a1 = area_triangulo(v1, v2)
        a2 = area_triangulo(v3, v4)
        a3 = area_triangulo(v1, v5)
        a4 = area_triangulo(v4, v6)
        soma_areas = (a1 + a2 + a3 + a4)
        areas = (/a1, a2, a3, a4/)

        ! volume do tetraedro
        volume = volume_tetraedro(v1, v2, v5)

        ! arquivo de saida
        open(2, file = 'tetra_out.dat', status = 'replace')
        write(2, *) volume
        write(2, *) soma_areas

        call ordena(areas)
        write(2, *) areas(1) ! impressao apenas de areas distintas
        do i = 2, 4
                if (areas(i) /= areas(i - 1)) then
                        write(2, *) areas(i)
                end if
        end do
        close(2)

end program exer04

real function area_triangulo(c1, c2)

        implicit none
        real, dimension(3) :: c1, c2, produto_vetorial
        real :: norma

        produto_vetorial(1) = (c1(2)*c2(3) - c1(3)*c2(2))
        produto_vetorial(2) = (c1(3)*c2(1) - c1(1)*c2(3))
        produto_vetorial(3) = (c1(1)*c2(2) - c1(2)*c2(1))

        norma = sqrt((produto_vetorial(1) ** 2) + (produto_vetorial(2) ** 2) + (produto_vetorial(3) ** 2))

        area_triangulo = (0.5) * norma

end function area_triangulo 

real function volume_tetraedro(c1, c2, c3)

       implicit none
       real, dimension(3) :: c1, c2, c3
       real :: produto_misto

       produto_misto = ((c1(2)*c2(3)*c3(1) + c1(3)*c2(1)*c3(2) + c1(1)*c2(2)*c3(3)) - &
                        (c1(3)*c2(2)*c3(1) + c1(1)*c2(3)*c3(2) + c1(2)*c2(1)*c3(3)))

       volume_tetraedro = (1./6) * abs(produto_misto)

end function volume_tetraedro

subroutine ordena(lista)

        implicit none
        real :: aux
        real, dimension(4) :: lista
        integer :: i, j

        do i = 1, 4
                j = i
                aux = lista(j)
                do while ((j > 1) .and. (lista(j - 1) > aux))
                        lista(j) = lista(j - 1)
                        j = j - 1
                end do
                lista(j) = aux
        end do

end subroutine ordena
