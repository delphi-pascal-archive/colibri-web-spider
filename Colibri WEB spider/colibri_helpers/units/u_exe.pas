// 010 u_exe
// 10 dec 2004

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

(*$r+*)

unit u_exe;
  interface

    function f_exe_path: String;
    function f_exe_pure_name: String;
    function f_exe_pure_project_name: String;

  implementation
    uses SysUtils, Forms
        , u_strings
    ;

    function f_exe_path: String;
      begin
        Result:= LowerCase(ExtractFilePath(Application.ExeName));
      end; // f_exe_path

    function f_exe_pure_name: String;
      begin
        Result:= LowerCase(ChangeFileExt(ExtractFileName(Application.ExeName), ''));
      end; // f_exe_pure_name

    function f_exe_pure_project_name: String;
      begin
        Result:= f_remove_start_if_start_is_equal_to(f_exe_pure_name, 'p_');;
      end; // f_exe_pure_project_name

    begin
    end.







































