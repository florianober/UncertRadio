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

function get_help_url(idstr, error) result(url)

    ! This subroutine is called when a user clicks on various Help buttons throughout the application.
    ! It returns the specific url associated with the button
    !

    use, intrinsic :: iso_c_binding,       only: c_int, c_null_ptr, c_null_char, c_new_line, c_ptr

    use ur_general_globals,                only: help_path
    use file_io,                           only: logger


    use chf,                               only: flfu
    use translation_module,                only: T => get_translation, get_language

    implicit none

    character(len=*), intent(in) :: idstr
    character(:), allocatable :: url
    character(:), allocatable, optional, intent(out) :: error

    logical                                :: ex
    integer                                :: i, pos
    character(len=128)                     :: topics(26)
    character(:), allocatable              :: idstring, home_url, lang, error_tmp

    !----------------------------------------------------------------------------------------------
    topics(1) = 'index.html | TBInfoDialog'
    topics(2) = 'index.html | Help_UR'
    topics(3) = 'doc_files/first_steps/options_dialog.html | DOptionsHelp'
    topics(4) = 'doc_files/first_steps/output_plots.html | HelpBS1'
    topics(5) = 'doc_files/first_steps/TABS/Procedures.html | HelpProcedure'
    topics(6) = 'doc_files/first_steps/TABS/Equations.html | HelpEquations'
    topics(7) = 'doc_files/first_steps/TABS/Values_Uncertainties.html | HelpValUnc'
    topics(8) = 'doc_files/first_steps/TABS/Uncertainty_Budget.html | HelpBudget'
    topics(9) = 'doc_files/first_steps/TABS/Results.html | TRButtonHelp'
    topics(10) = 'doc_files/batch_mode/serial_evaluation.html | HelpSE'
    topics(11) = 'doc_files/batch_mode/processing_projects_batch_mode.html | HelpBEV'
    topics(12) = 'doc_files/batch_mode/run_all_tests.html | BTHelp'
    topics(13) = 'doc_files/special_methods/linear_least_square.html | HelpLinfit'
    topics(14) = 'doc_files/special_methods/calibration_curve.html | HelpKalib'
    topics(15) = 'doc_files/special_methods/gamma_lines.html | HelpGspk1'
    topics(16) = 'doc_files/special_methods/monte_carlo.html | HelpMC1'
    topics(17) = 'doc_files/misc/text_fields_equations.html | HelpTextEQ'
    topics(18) = 'doc_files/misc/dialog_decay_curve.html | HelpDecayInput'
    topics(19) = 'doc_files/misc/dialog_def_curve_model.html | HelpDecayModel'
    topics(20) = 'doc_files/first_steps/problem_advice.html | TBProblems'
    topics(21) = 'doc_files/special_methods/confidence_ellipses.html | HelpELI'
    topics(22) = 'doc_files/special_methods/data_sets_mean.html | MDHelp'
    topics(23) = 'doc_files/special_methods/short-lived_nuclide.html | BinPoiHelp'
    topics(24) = 'doc_files/special_methods/special_distributions.html | HelpDistrib'
    topics(25) = 'doc_files/special_methods/aggregating_activities.html | HelpSumEval'
    topics(26) = 'doc_files/first_steps/example_projects.html | HelpExamples'

    error_tmp = ""
    url = ""
    home_url = help_path // 'final/html/index.html'

    idstring = idstr

    inquire(file=flfu(home_url), exist=ex)
    if (.not. ex) then
        error_tmp = T("Could find the help files") // ":" // c_new_line // home_url
        call logger(66, error_tmp)

    else
        if (get_language() == 'en') then
            lang = ''
        else
            lang = get_language() // '/'
        end if

        ! search for the correct topic that is linked to the ButtonID.
        do i=1, size(topics)
            pos = index(topics(i), '|')

            if(idstring == trim(adjustl(topics(i)(pos+1:)))) then
                url = help_path // 'final/html/' // lang // trim(topics(i)(1:pos-1))
                inquire(file=flfu(url), exist=ex)
                if (.not. ex) then
                    error_tmp = "Help: Could not find '" // url // "'"
                    call logger(66, error_tmp)
                    url = home_url
                end if
            end if
        end do

        ! check if an url is found
        if (url == "") then
            url = home_url
            error_tmp = "Could not find url for button id '" // idstring // "'"
            call logger(66, error_tmp)
        end if
    end if

    if (present(error)) then
        error = " "
        error = error_tmp
    end if

end function get_help_url
