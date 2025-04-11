!-------------------------------------------------------------------------------------------------!
! This file is part of UncertRadio.
!
!    UncertRadio is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    UncertRadio is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with UncertRadio. If not, see <http://www.gnu.org/licenses/>.
!
!-------------------------------------------------------------------------------------------------!


module gtk_prepare_dialog

!     Copyright (C) 2014-2025  Günter Kanisch, Florian Ober


    ! This module contains subroutines to prepare a gtk dialog before showing it
    ! These subroutine are mainly called by handler routines
    !
    use, intrinsic :: iso_c_binding,  only: c_int, c_null_char, &
                                            c_null_ptr,c_associated, &
                                            c_f_pointer, c_ptr, c_char, &
                                            c_double, c_loc, c_float

    use UR_params,          only: rn, EPS1MIN


    use gtk,                only: TRUE, FALSE
    use gtk_sup,            only: c_f_string, clear_gtktreeiter
    use top,                only: idpt


contains

    !

end module gtk_prepare_dialog
