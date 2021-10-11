// 002 u_label
// 11 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

(*$r+*)

unit u_label;
  interface
    uses Classes
        , StdCtrls // tLabel
        , Controls, ExtCtrls;

    function f_c_create_label(p_name, p_caption: String;
        p_c_owner: tComponent; p_c_parent: tWinControl;
        p_align: tAlign; p_left, p_top, p_width, p_height: Integer): tLabel;

  implementation
    uses SysUtils, u_c_display;

    function f_c_create_label(p_name, p_caption: String;
        p_c_owner: tComponent; p_c_parent: tWinControl;
        p_align: tAlign; p_left, p_top, p_width, p_height: Integer): tLabel;
      begin
        Result:= tLabel.Create(p_c_owner);
        with Result do
        begin
          Parent:= p_c_parent;
          Name:= p_name;
          Caption:= p_caption;
          Align:= p_align;
          if p_left<> -1
            then Left:= p_left;
          if p_top<> -1
            then Top:= p_top;
          if p_width<> -1
            then Width:= p_width;  
          if p_height<> -1
            then Height:= p_height;
        end; // with l_c_status_panel
      end; // f_c_create_label

    begin // u_c_control_leaf
    end. // u_c_control_leaf
