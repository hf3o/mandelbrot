program mandelbrot
    implicit none
    integer, parameter :: nx = 800        ! Szerokość obrazu
    integer, parameter :: ny = 600        ! Wysokość obrazu
    integer, parameter :: max_iter = 100  ! Maksymalna liczba iteracji
    real, parameter :: x_min = -2.5, x_max = 1.0  ! Zakres osi x
    real, parameter :: y_min = -1.5, y_max = 1.5  ! Zakres osi y
    real :: x, y, x0, y0, xtemp
    integer :: i, j, iter, unit
    integer :: r, g, b                    ! Składowe koloru RGB

    ! Otwarcie pliku PPM
    open(newunit=unit, file='mandelbrot.ppm', status='replace', action='write')
    
    ! Nagłówek PPM (P3 - tekstowy format RGB)
    write(unit, '(A)') 'P3'
    write(unit, '(I0, 1X, I0)') nx, ny
    write(unit, '(I0)') 255  ! Maksymalna wartość koloru

    ! Generowanie fraktala
    do j = 1, ny
        y0 = y_min + (y_max - y_min) * real(j-1) / real(ny-1)
        do i = 1, nx
            x0 = x_min + (x_max - x_min) * real(i-1) / real(nx-1)
            x = 0.0
            y = 0.0
            iter = 0

            ! Iteracja zbioru Mandelbrota
            do while (iter < max_iter .and. x*x + y*y <= 4.0)
                xtemp = x*x - y*y + x0
                y = 2.0*x*y + y0
                x = xtemp
                iter = iter + 1
            end do

            ! Kolorowanie piksela
            if (iter == max_iter) then
                r = 0   ! Czarny dla punktów w zbiorze
                g = 0
                b = 0
            else
                ! Proste mapowanie koloru na podstawie liczby iteracji
                r = mod(iter * 5, 255)       ! Czerwony
                g = mod(iter * 10, 255)      ! Zielony
                b = mod(iter * 15, 255)      ! Niebieski
            end if

            ! Zapis piksela do pliku
            write(unit, '(I0, 1X, I0, 1X, I0)') r, g, b
        end do
    end do

    ! Zamknięcie pliku
    close(unit)
    print *, 'Plik mandelbrot.ppm został wygenerowany.'

end program mandelbrot
