program exer05

        implicit none

        ! declaracao das variaveis
        real(16) :: precisao, e = 1000000_16, lambda
        integer :: n, i, j ! n: dimensao da matriz
        real(16), allocatable, dimension(:,:) :: matriz
        real(16), allocatable, dimension(:) :: vec01, vec02, vec03

        ! leitura das entradas
        read *, precisao
        read *, n
        allocate(matriz(n, n)) ! alocacao da matriz
        read *, matriz
        print *, matriz

        ! valor do chute inicial dos vetores (vec01 = (1, 0, ..., 0))
        allocate(vec01(n), vec02(n), vec03(n)) ! alocacao dos vetores
        vec01 = 0
        vec01(1) = 1

        ! operacoes
        do while (abs(e) > precisao)
                vec03 = matmul(matriz, vec01)
                vec02 = vec03 / (sqrt(dot_product(vec03, vec03)))
                e = (dot_product(vec02, matmul(matriz, vec02)) - lambda)
                vec01 = vec02
                lambda = dot_product(vec02, matmul(matriz, vec02))
        end do

        ! saidas
        print *, lambda
        do i = 1, size(vec02)
                print *, vec02(i)
        end do

        ! liberacao da memoria alocada
        deallocate(matriz, vec01, vec02, vec03)

end program exer05
