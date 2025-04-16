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


    use gtk,                only: TRUE, FALSE
    use gtk_sup,            only: c_f_string, clear_gtktreeiter
    use top,                only: idpt

    private
    public :: prepare_batest_dialog, &
              prepare_options_dialog

contains

    subroutine prepare_batest_dialog()
        !
        !
        use gtk,                    only: gtk_file_chooser_set_filename
        use ur_general_globals,     only: work_path, results_path
        use UR_params,              only: BATEST_REF_FILE, BATEST_OUT
        !-----------------------------------------------------------------------------------------!
        implicit none

        integer(c_int)                 :: resp
        type(c_ptr), save              :: button1 = c_null_ptr, &
                                          button2 = c_null_ptr
        !-----------------------------------------------------------------------------------------!
        if (.not. c_associated(button1)) button1 = idpt('BTchooserButton_1')
        if (.not. c_associated(button2)) button2 = idpt('BTchooserButton_2')

        resp = gtk_file_chooser_set_filename(button1, &
                trim(work_path) // trim(Batest_ref_file) // c_null_char)
        resp = gtk_file_chooser_set_filename(button2, &
                trim(results_path) // trim(Batest_out) // c_null_char)

    end subroutine prepare_batest_dialog

    !---------------------------------------------------------------------------------------------!

    subroutine prepare_options_dialog()
        !
        ! external
        use gtk,                    only: gtk_widget_set_sensitive

        ! UR parameters
        ! use UR_params,              only: zero, one

        ! UR globals
        use UR_DLIM,                only: kalpha, kbeta, GamDistAdd, &
                                          W1minusG, NWGMethode
        use UR_Gleich_globals,      only: coverf, coverin
        use ur_general_globals,     only: sListSeparator

        ! UR functions
        use Rout,                   only: wdputentrydouble, wdputentrystring, wdsetcomboboxact, &
                                          wdsetcheckbutton
        use brandt,                 only: pnorm
        use translation_module,     only: get_language
        use color_theme,            only: get_theme_name
        !-----------------------------------------------------------------------------------------!
        implicit none
        integer(c_int)                 :: lang_i

        !-----------------------------------------------------------------------------------------!

        ! when updateing these entryfields, the handler to calculate alpha and beta are called
        call WDPutEntryDouble('entryOptKalpha', kalpha,'(f10.8)')
        call WDPutEntryDouble('entryOptKbeta', kbeta,'(f10.8)')

        call WDPutEntryDouble('entryOptCoverf', coverf,'(f5.2)')
        call WDPutEntryDouble('entryOptCoverIn', coverin,'(f5.2)')
        call WDPutEntryDouble('entryOpt1minusG', W1minusG,'(f5.3)')
        call WDPutEntryString('entryOptDLMethod', trim(NWGMethode))

        call WDPutEntryDouble('entryOptGamDistAdd', GamDistAdd,'(f3.1)')
        if(sListSeparator == ';') call WDSetComboboxAct('comboboxtextListSep',1)
        if(sListSeparator == ',') call WDSetComboboxAct('comboboxtextListSep',2)

        lang_i = 2
        if(get_language() == 'de') then
            lang_i = 1
        else if(get_language() == 'fr') then
            lang_i = 3
        end if
        call WDSetComboboxAct('comboboxLangg', lang_i)

        if (get_theme_name() == "contrast") then
            call WDSetCheckButton('check_contrastmode', 1)
        else
            call WDSetCheckButton('check_contrastmode', 0)
        end if

        call gtk_widget_set_sensitive(idpt('DOptionsOK'), FALSE)   ! 13.4.2023


    end subroutine prepare_options_dialog

    !-----------------------------------------------------------------------------------------!

    subroutine prepare_fontselection_dialog(settings)
        !
        !
        use gtk,                    only: gtk_widget_set_sensitive, gtk_font_button_set_font_name
        use UR_gtk_window_types,    only: KSetting

        !-----------------------------------------------------------------------------------------!
        implicit none

        type(KSetting), intent(in)     :: settings
        integer(c_int)                 :: resp_id, i
        type(c_ptr), save              :: save_button = c_null_ptr
        !-----------------------------------------------------------------------------------------!
        if (.not. c_associated(save_button)) save_button = idpt('buttonFBSave')

        call gtk_widget_set_sensitive(idpt('buttonFBSave'), FALSE)
        do i=1, settings%nprops
            if(trim(settings%sproperty(i)) == 'gtk-font-name') then
                resp_id = gtk_font_button_set_font_name(idpt('fontbutton1'), &
                    trim(settings%sproperty_val(i))//c_null_char)
                exit
            end if
        end do

    end subroutine prepare_fontselection_dialog

end module gtk_prepare_dialog
