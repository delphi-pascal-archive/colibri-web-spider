// 003 u_c_find_memo
// 11 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

(*$r+*)

unit u_c_find_memo;
  interface
    uses Graphics
        , Classes // tShiftState
        , Controls // tMouseButton
        , StdCtrls
        , ComCtrls
        , ExtCtrls // tPanel, tPaintBox
        , u_c_basic_object;

    type c_find_memo= class(c_basic_object)
                        m_c_parent_component_ref: tWinControl;
                        m_save_path: String;

                        // -- optional to avoid erasing the selected part
                        // -- (could also use a ReadOnly memo)
                        m_do_hilite_result: Boolean;

                        m_c_panel: tPanel;
                          m_c_text_length_label, m_c_text_line_count_label: tLabel;
                          m_c_find_edit: tEdit;
                          m_c_find_button, m_c_next_button, m_c_sort_button: tButton;
                          m_c_save_edit: tEdit;
                        m_c_memo: tMemo;

                        m_find_string: String;
                        m_find_length: Integer;

                        m_text: String;
                        m_text_index: Integer;
                        m_text_length: Integer;

                        m_did_hit_control: Boolean;

                        Constructor create_find_memo(p_name: String;
                            p_c_parent_component_ref: tWinControl;
                            p_save_path: String);

                        procedure set_panel_color(p_color: Integer);
                        function f_memo_line_of_index(p_text_index: Integer): Integer;
                        procedure set_memo_top_line_index(p_top_line_index: Integer);
                        procedure find_next_occurence;

                        procedure handle_find_edit_keypress(p_c_sender: TObject;
                            var pv_key: Char);
                        procedure handle_memo_and_find_edit_keydown(p_c_sender: TObject;
                            var pv_scan_code: Word; p_shift_state: TShiftState);
                        procedure handle_next_click(p_c_sender: tObject);
                        procedure handle_sort_click(p_c_sender: tObject);
                        procedure handle_memo_change(p_c_sender: tObject);
                        procedure handle_save_edit_keypress(p_c_sender: TObject;
                            var pv_key: Char);

                        Destructor Destroy; Override;
                      end; // c_find_memo

  implementation
    Uses Windows, Messages, SysUtils, u_c_display, u_characters
        , u_panel, u_button, u_edit, u_label
        ;

    // -- c_find_memo

    Constructor c_find_memo.create_find_memo(p_name: String;
        p_c_parent_component_ref: tWinControl;
        p_save_path: String);

      procedure create_panel;
        const k_button_height= 25;
        var l_x: Integer;
        begin
          m_c_panel:= f_c_create_panel('panel', '',
            m_c_parent_component_ref, m_c_parent_component_ref,
            alTop, 0, k_button_height+ 2, clBtnFace);

          l_x:= 15;

          m_c_text_length_label:= f_c_create_label('tlength', 'length',
              m_c_panel, m_c_panel, alNone, l_x, 2+ 4, -1, -1); // k_button_height- 3);
          Inc(l_x, m_c_text_length_label.Width+ 5);

          m_c_text_line_count_label:= f_c_create_label('tline', 'lines  ',
              m_c_panel, m_c_panel, alNone, l_x, 2+ 4, -1, -1); // k_button_height- 3);
          Inc(l_x, m_c_text_line_count_label.Width+ 5);

          Inc(l_x, 10);
          m_c_find_edit:= f_c_create_edit('edit', 'find',
              m_c_panel, m_c_panel, l_x, 2, 100, k_button_height);
          with m_c_find_edit do
          begin
            OnKeyPress:= handle_find_edit_keypress;
            OnKeyDown:= handle_memo_and_find_edit_keydown;

            Inc(l_x, Width+ 5);
          end; // with m_c_find_edit

          m_c_next_button:= f_c_create_button('next', '>',
              m_c_panel, m_c_panel, l_x, 2, 20, k_button_height);
          Inc(l_x, m_c_next_button.Width+ 5);
          m_c_next_button.OnClick:= handle_next_click;

          m_c_sort_button:= f_c_create_button('sort', 'sort',
              m_c_panel, m_c_panel, l_x, 2, 40, k_button_height);
          Inc(l_x, m_c_sort_button.Width+ 5);
          m_c_sort_button.OnClick:= handle_sort_click;

          Inc(l_x, 10);
          m_c_save_edit:= f_c_create_edit('save', 'save.txt',
              m_c_panel, m_c_panel, l_x, 2, 100, k_button_height);
          with m_c_save_edit do
          begin
            OnKeyPress:= handle_save_edit_keypress;

            Inc(l_x, Width+ 5);
          end; // with m_c_save_edit
        end; // create_panel

      begin // create_find_memo
        Inherited create_basic_object(p_name);

        m_c_parent_component_ref:= p_c_parent_component_ref;
        m_save_path:= p_save_path;

        create_panel;

        m_c_memo:= tMemo.Create(m_c_parent_component_ref);
        with m_c_memo do
        begin
          Parent:= m_c_parent_component_ref;
          Align:= alClient;
          ScrollBars:= ssVertical;

          OnKeyDown:= handle_memo_and_find_edit_keydown;
          OnChange:= handle_memo_change;
        end; // with m_c_memo
      end; // create_find_memo

    procedure c_find_memo.set_panel_color(p_color: Integer);
      begin
        m_c_panel.Color:= p_color;
      end; // set_panel_color

    function c_find_memo.f_memo_line_of_index(p_text_index: Integer): Integer;
      var l_line_index: Integer;
          l_running_text_index: Integer;
      begin
        Result:= 0;
        l_running_text_index:= 0;
        with m_c_memo do
          for l_line_index:= 0 to Lines.Count- 1 do
            if l_running_text_index> p_text_index
              then begin
                  Result:= l_line_index- 1;
                  Break;
                end
              else Inc(l_running_text_index, Length(Lines[l_line_index])+ 2);
      end; // f_memo_line_of_index

    procedure c_find_memo.set_memo_top_line_index(p_top_line_index: Integer);
      var l_first_visible_line: Integer;
      begin
        with m_c_memo do
        begin
          l_first_visible_line:= Perform(em_GetFirstVisibleLine, 0, 0);
          if l_first_visible_line<> p_top_line_index
            then Perform(em_LineScroll, 0, p_top_line_index- l_first_visible_line);
        end;
      end; // set_memo_top_line_index

    procedure c_find_memo.find_next_occurence;
        // -- find string

      function f_find_rest(p_index: Integer): Boolean;
        var l_word_index: Integer;
        begin
          l_word_index:= 2;
          while (l_word_index<= m_find_length)
              and (p_index+ l_word_index<= m_text_length)
              and (m_find_string[l_word_index]= m_text[p_index+ l_word_index- 1]) do
            inc(l_word_index);

          Result:= l_word_index> Length(m_find_string);
        end; // f_find_rest

      var l_first_character: Char;

      begin // find_next_occurence
        with m_c_memo do
        begin
          display('> find '+ IntToStr(m_text_index));
          Inc(m_text_index, m_find_length);

          l_first_character:= m_find_string[1];

          while m_text_index< m_text_length do
          begin
            if m_text[m_text_index]= l_first_character
              then
                if f_find_rest(m_text_index)
                  then begin
                      // Inc(m_text_index, Length(m_find_string)- 1);
                      set_memo_top_line_index(f_memo_line_of_index(m_text_index));
                      SelStart:= m_text_index- 1;
                      if m_do_hilite_result
                        then SelLength:= m_find_length;
                      set_panel_color(clLime);
                      display('< find: found at '+ IntToHex(m_text_index, 4));
                      exit;
                    end;

            inc(m_text_index);
          end; // while pv_index

          display('< find');
        end; // with m_c_memo do
        set_panel_color(clRed);
      end; // find_next_occurence

    procedure c_find_memo.handle_find_edit_keypress(p_c_sender: TObject; var pv_key: Char);
      begin
        if pv_key= k_return
          then begin
              pv_key:= chr(0);
              m_find_string:= m_c_find_edit.Text;
              m_find_length:= Length(m_find_string);

              m_text:= m_c_memo.Text;
              m_text_index:= 1- m_find_length;
              m_text_length:= Length(m_text);

              find_next_occurence;
            end;
      end; // handle_find_edit_keypress

    procedure c_find_memo.handle_memo_and_find_edit_keydown(p_c_sender: TObject;
        var pv_scan_code: Word; p_shift_state: TShiftState);
      begin
        case pv_scan_code of
          vk_control : m_did_hit_control:= True;
          vk_escape : begin
                        set_panel_color(clBtnFace);
                        m_c_memo.SelLength:= 0;
                      end;
          Ord('L') : if m_did_hit_control
                       then begin
                           // -- no beep. Does not work ?
                           pv_scan_code:= 0;

                           find_next_occurence;
                           // -- do not cancel the control, if keeps finger
                           // --   on Ctrl for repeated "find next"
                           // m_did_hit_control:= False;
                         end;
          Ord('F') : if m_did_hit_control
                       then begin
                           // -- no beep
                           pv_scan_code:= 0;

                           m_c_find_edit.SetFocus;
                           // -- do not cancel the control, if keeps finger
                           // --   on Ctrl for repeated "find next"
                           // m_did_hit_control:= False;
                         end;
          else
            m_did_hit_control:= False;
        end; // case pv_scan_code
      end; // handle_memo_and_find_edit_keydown

    procedure c_find_memo.handle_next_click(p_c_sender: tObject);
      begin
        find_next_occurence;
      end; // handle_next_click

    procedure c_find_memo.handle_sort_click(p_c_sender: tObject);
      var l_c_stringlist: tStringList;
      begin
        l_c_stringlist:= tStringList.Create;
        with l_c_stringlist do
        begin
          Assign(m_c_memo.Lines);
          Sorted:= True;
          m_c_memo.Lines.Assign(l_c_stringlist);
          Free;
        end; // with l_c_stringlist
      end; // handle_sort_click

    procedure c_find_memo.handle_memo_change(p_c_sender: tObject);
      begin
        m_c_text_length_label.Caption:= IntToStr(Length(m_c_memo.Text));
        m_c_text_line_count_label.Caption:= IntToStr(m_c_memo.Lines.Count);
      end; // handle_memo_change

    procedure c_find_memo.handle_save_edit_keypress(p_c_sender: TObject; var pv_key: Char);
      begin
        if pv_key= k_return
          then begin
              pv_key:= chr(0);
              m_c_memo.Lines.SaveToFile(m_save_path+ m_c_save_edit.Text);
              display('save '+ m_save_path+ m_c_save_edit.Text);
            end;
      end; // handle_save_edit_keypress

    Destructor c_find_memo.Destroy;
      begin
        Inherited;
      end; // Destroy

    begin // u_c_find_memo
    end. // u_c_find_memo
_memo
