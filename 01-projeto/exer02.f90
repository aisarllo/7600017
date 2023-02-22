program exer02

        implicit none

        ! declaracao das variaveis
        real(4), dimension(4) :: x = (/0.1, 0.2, 0.3, 0.4/)
        integer :: calc_fatorial
        integer :: TERMO

        ! precisao simples
        real(4) :: sen101s, sen201s, termo01s ! para 0.1
        real(4) :: sen102s, sen202s, termo02s ! para 0.2
        real(4) :: sen103s, sen203s, termo03s ! para 0.3
        real(4) :: sen104s, sen204s, termo04s ! para 0.4

        ! precisao dupla
        real(8) :: sen101d, sen201d, termo01d ! para 0.1
        real(8) :: sen102d, sen202d, termo02d ! para 0.2
        real(8) :: sen103d, sen203d, termo03d ! para 0.3
        real(8) :: sen104d, sen204d, termo04d ! para 0.4

        ! PRECISÃO SIMPLES
        ! x = 0.1
        TERMO = 1
        sen201s = x(1)
        do while (sen101s /= sen201s)
                sen101s = sen201s
                termo01s = (((-1) ** TERMO) * (x(1) ** (2 * TERMO + 1))) / (calc_fatorial((2 * TERMO) + 1))
                sen201s = sen101s + termo01s
                TERMO = TERMO + 1     
        end do

        termo01s = sen201s-(sen101s-((((-1) ** (TERMO - 2)) * (x(1) ** (2 * (TERMO - 2) + 1))) &
        / (calc_fatorial((2 * (TERMO - 2)) + 1))))

        print *, x(1), abs(termo01s/sen101s)

        ! x = 0.2
        TERMO = 1
        sen202s = x(2)
        do while (sen102s /= sen202s)
                sen102s = sen202s
                termo02s = (((-1) ** TERMO) * (x(2) ** (2 * TERMO + 1))) / (calc_fatorial((2 * TERMO) + 1))
                sen202s = sen102s + termo02s
                TERMO = TERMO + 1     
        end do

        termo02s = sen202s - (sen102s - ((((-1)**(TERMO - 2)) * (x(2) ** (2 * (TERMO - 2) + 1))) &
        / (calc_fatorial((2 * (TERMO - 2)) + 1))))

        print *, x(2), abs(termo02s/sen102s)

        ! x = 0.3
        TERMO = 1
        sen203s = x(3)
        do while (sen103s /= sen203s)
                sen103s = sen203s
                termo03s = (((-1) ** TERMO) * (x(3) ** (2 * TERMO + 1))) / (calc_fatorial((2 * TERMO) + 1))
                sen203s = sen103s + termo03s
                TERMO = TERMO + 1     
        end do

        termo03s = sen203s - (sen103s - ((((-1)**(TERMO - 2)) * (x(3) ** (2 * (TERMO - 2) + 1))) &
        / (calc_fatorial((2 * (TERMO - 2)) + 1))))

        print *, x(3), abs(termo03s/sen103s)

        ! x = 0.4
        TERMO = 1
        sen204s = x(4)
        do while (sen104s /= sen204s)
                sen104s = sen204s
                termo04s = (((-1) ** TERMO) * (x(4) ** (2 * TERMO + 1))) / (calc_fatorial((2 * TERMO) + 1))
                sen204s = sen104s + termo04s
                TERMO = TERMO + 1     
        end do

        termo04s = sen204s - (sen104s - ((((-1)**(TERMO - 2)) * (x(4) ** (2 * (TERMO - 2) + 1))) &
        / (calc_fatorial((2 * (TERMO - 2)) + 1))))

        print *, x(4), abs(termo04s/sen104s)

        ! PRECISÃO DUPLA
        ! x = 0.1
        TERMO = 1
        sen201d = x(1)
        do while (sen101d /= sen201d)
                sen101d = sen201d
                termo01d = (((-1) ** TERMO) * (x(1) ** (2 * TERMO + 1))) / (calc_fatorial((2 * TERMO) + 1))
                sen201d = sen101d + termo01d
                TERMO = TERMO + 1
        end do

        termo01d = sen201d - (sen101d - ((((-1) ** (TERMO - 2)) * (x(1) ** (2 * (TERMO - 2) + 1))) &
        / (calc_fatorial((2 * (TERMO - 2)) + 1))))

        print *, x(1), abs(termo01d/sen101d)

        ! x = 0.2
        TERMO = 1
        sen202d = x(2)
        do while (sen102d /= sen202d)
                sen102d = sen202d
                termo02d = (((-1) ** TERMO) * (x(2) ** (2 * TERMO + 1))) / (calc_fatorial((2 * TERMO) + 1))
                sen202d = sen102d + termo02d
                TERMO = TERMO + 1
        end do

        termo02d = sen202d - (sen102d - ((((-1) ** (TERMO - 2)) * (x(2) ** (2 * (TERMO - 2) + 1))) &
        / (calc_fatorial((2 * (TERMO - 2)) + 1))))

        print *, x(2), abs(termo02d/sen102d)
 
        ! x = 0.3
        TERMO = 1
        sen203d = x(3)
        do while (sen103d /= sen203d)
                sen103d = sen203d
                termo03d = (((-1) ** TERMO) * (x(3) ** (2 * TERMO + 1))) / (calc_fatorial((2 * TERMO) + 1))
                sen203d = sen103d + termo03d
                TERMO = TERMO + 1
        end do

        termo03d = sen203d - (sen103d - ((((-1) ** (TERMO - 2)) * (x(3) ** (2 * (TERMO - 2) + 1))) &
        / (calc_fatorial((2 * (TERMO - 2)) + 1))))

        print *, x(3), abs(termo03d/sen103d)

        ! x = 0.4
        TERMO = 1
        sen204d = x(4)
        do while (sen104d /= sen204d)
                sen104d = sen204d
                termo04d = (((-1) ** TERMO) * (x(4) ** (2 * TERMO + 1))) / (calc_fatorial((2 * TERMO) + 1))
                sen204d = sen104d + termo04d
                TERMO = TERMO + 1
        end do

        termo04d = sen204d - (sen104d - ((((-1) ** (TERMO - 2)) * (x(4) ** (2 * (TERMO - 2) + 1))) &
        / (calc_fatorial((2 * (TERMO - 2)) + 1))))
 
        print *, x(4), abs(termo04d/sen104d)
 
end program exer02

integer function calc_fatorial(termo)
        integer :: termo
        calc_fatorial = 1

        do while (termo /= 1)
                calc_fatorial = calc_fatorial * termo 
                termo = termo - 1
        end do

        calc_fatorial = calc_fatorial
end function calc_fatorial
