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
module PMD
    use UR_types
    implicit none
    interface

        module recursive subroutine ProcMainDiag(widget)
            type(c_ptr), intent(in)            :: widget   ! index of widget in the list of clobj
        end subroutine ProcMainDiag


        module subroutine GamSymList
        end subroutine GamSymList


        module subroutine GamPeakvals
        end subroutine GamPeakvals


        module subroutine AdjustRemoveTVRows(numrows_marked)
            use, intrinsic :: iso_c_binding,        only: c_int
            integer(c_int),intent(in) ::numrows_marked
        end subroutine AdjustRemoveTVRows

    end interface

end module PMD
