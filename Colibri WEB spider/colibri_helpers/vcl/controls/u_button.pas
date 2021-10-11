// 002 u_button
// 11 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

(*$r+*)

unit u_button;
  interface
    uses Classes, Controls, StdCtrls
        , Buttons // speedbutton
        ;

    function f_c_create_button(p_name, p_caption: String;
        p_c_owner: tComponent; p_c_parent: tWinControl;
        p_left, p_top, p_width, p_height: Integer): tButton;
    function f_c_create_speed_button(p_name, p_caption: String;
        p_c_owner: tComponent; p_c_parent: tWinControl;
        p_left, p_top, p_width, p_height: Integer;
        p_allow_all_up: Boolean; p_group_index: Integer): tSpeedButton;

  implementation
    uses SysUtils, u_c_display;

    function f_c_create_button(p_name, p_caption: String;
        p_c_owner: tComponent; p_c_parent: tWinControl;
        p_left, p_top, p_width, p_height: Integer): tButton;
      begin
        Result:= tButton.Create(p_c_owner);
        with Result do
        begin
          Parent:= p_c_parent;

          Name:= p_name;
          Caption:= p_caption;

          if p_left<> -1
            then Left:= p_left;
          if p_top<> -1
            then Top:= p_top;
          if p_height<> -1
            then Height:= p_height;
          if p_width<> -1
            then Width:= p_width;
        end; // with Result
      end; // f_c_create_button

    function f_c_create_speed_button(p_name, p_caption: String;
        p_c_owner: tComponent; p_c_parent: tWinControl;
        p_left, p_top, p_width, p_height: Integer;
        p_allow_all_up: Boolean; p_group_index: Integer): tSpeedButton;
      begin
        Result:= tSpeedButton.Create(p_c_owner);
        with Result do
        begin
          Parent:= p_c_parent;

          Name:= p_name;
          Caption:= p_caption;

          if p_left<> -1
            then Left:= p_left;
          if p_top<> -1
            then Top:= p_top;
          if p_height<> -1
            then Height:= p_height;
          if p_width<> -1
            then Width:= p_width;
          AllowAllUp:= p_allow_all_up;
          GroupIndex:= p_group_index;  
        end; // with Result
      end; // f_c_create_speed_button

    begin // u_c_control_leaf
    end. // u_c_control_leaf
