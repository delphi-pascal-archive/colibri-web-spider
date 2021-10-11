// 002 u_edit
// 11 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

(*$r+*)

unit u_edit;
  interface
    uses Classes
        , StdCtrls // tEdit
        , Controls // tWinControl
        // , ExtCtrls
        ;

    function f_c_create_edit(p_name, p_text: String;
        p_c_owner: tComponent; p_c_parent: tWinControl;
        p_left, p_top, p_width, p_height: Integer): tEdit;

  implementation
    uses SysUtils, u_c_display;

    function f_c_create_edit(p_name, p_text: String;
        p_c_owner: tComponent; p_c_parent: tWinControl;
        p_left, p_top, p_width, p_height: Integer): tEdit;
      begin
        Result:= tEdit.Create(p_c_owner);
        with Result do
        begin
          Parent:= p_c_parent;
          Name:= p_name;
          Text:= p_text;
          if p_left<> -1
            then Left:= p_left;
          if p_top<> -1
            then Top:= p_top;
          if p_height<> -1
            then Height:= p_height;
          if p_width<> -1
            then Width:= p_width;
        end; // with l_c_status_panel
      end; // f_c_create_label

    begin // u_c_control_leaf
    end. // u_c_control_leaf
