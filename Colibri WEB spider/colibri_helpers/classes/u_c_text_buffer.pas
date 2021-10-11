// 002 u_c_text_buffer
// 07 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

(*$r+*)

unit u_c_text_buffer;
  interface
    uses Classes, u_types_constants, u_characters
      , u_c_basic_object
      , u_c_line
      ;

    type c_text_buffer= class(c_basic_object)
                          // public
                          m_oa_text_buffer: t_oa_characters;
                          m_buffer_size, m_buffer_index: Integer;

                          // -- incremented by f_read_line only
                          m_line_number: Integer;

                          m_has_line_feed: Boolean;

                          m_c_line: c_line;

                          constructor create_text_buffer(p_name: String;
                              p_buffer_size: Integer);

                          procedure assign_buffer(p_pt: Pointer; p_size: Integer);

                          function f_read_line: Boolean; virtual;
                          function f_test_line: Boolean; virtual;
                          function f_peek_line: String;

                          function f_end_of_text: boolean; virtual;

                          procedure list; virtual;
                          procedure list_strings(p_c_strings: tStrings);
                          procedure dump_hex(p_begin, p_end: Integer);
                          function f_ascii_signature: Integer;

                          // -- could use the name f_copy
                          function f_extract_string_start_length(p_start, p_length: Integer): String;
                          function f_extract_string_start_end(p_start, p_end: Integer): String;
                          function f_extract_string_start_end_and_check(p_start, p_end: Integer;
                              var pv_string: String): Boolean;
                          function f_extract_string_start_end_max(p_start, p_end: Integer): String;

                          function f_end_of_line(p_index: Integer): Boolean;
                          function f_end_of_line_size(p_index: Integer): Integer;
                          procedure skip_alternate_return_line_feed(var pv_index: Integer);
                          procedure skip_end_of_line(var pv_index: Integer);

                          function f_check_string(p_text_index: Integer; p_text: String): Boolean;
                          procedure replace_string(p_start_index: Integer; p_original, p_replacement: String);
                          function f_replace_string(p_start_index, p_original_length: Integer; p_replacement: String): Boolean;

                          function f_load_from_file(p_full_file_name: String): Boolean;
                          procedure save_to_file(p_full_file_name: String);

                          destructor Destroy; Override;
                      end;

  implementation
    uses SysUtils, u_c_display, u_display_hex_2
        , u_file
        ;

    constructor c_text_buffer.create_text_buffer(p_name: String;
        p_buffer_size: Integer);
      begin
        Inherited create_basic_object(p_name);

        if p_buffer_size<> 0
          then begin
              m_buffer_size:= p_buffer_size;
              SetLength(m_oa_text_buffer, m_buffer_size);
            end;
            
        m_c_line:= c_line.create_basic_object('e_'+ p_name+ '_line');
      end; // create_text_buffer

    procedure c_text_buffer.assign_buffer(p_pt: Pointer; p_size: Integer);
      begin
        m_buffer_size:= p_size;
        SetLength(m_oa_text_buffer, m_buffer_size);
        if m_buffer_size> 0
          then Move(p_pt^, m_oa_text_buffer[0], m_buffer_size);
        m_buffer_index:= 0;
        m_line_number:= 0;
      end; // assign_buffer

    function c_text_buffer.f_read_line: Boolean;
        // -- False if no more text (EOT)
      var l_start_index: integer;
      begin
        l_start_index:= m_buffer_index;

        while (m_buffer_index< m_buffer_size) and not (m_oa_text_buffer[m_buffer_index] in [k_return, k_line_feed]) do
          m_buffer_index:= m_buffer_index+ 1;

        with m_c_line do
        begin
          m_buffer_index_of_start:= l_start_index;

          m_line_size:= m_buffer_index- l_start_index;
          // display('line_size '+ IntToStr(m_buffer_index)+ ' '+ IntToStr(m_line_size));
          // if m_line_size< 0
          //  then display_bug_halt('f_read_line, size '+ IntToStr(m_buffer_index));

          if m_line_size= 0
            then begin
                m_the_line:= '';
                Result:= True;
              end
            else
              if (m_line_size>= 1) and (m_oa_text_buffer[l_start_index]= chr(26))
                then begin
                    // Destroy Ctrl-Z of MS DOS
                    m_line_size:= 0;
                    m_the_line:= '';
                    Result:= False;
                  end
                else begin
                    SetLength(m_the_line, m_line_size);
                    move(m_oa_text_buffer[l_start_index], m_the_line[1], m_line_size);
                    m_line_index:= 1;

                    Result:= True;
                  end;

          // -- the c_text_buffer line number
          inc(m_line_number);
          m_line_line_number:= m_line_number;

{
          writeln('>', m_the_line, '<');
          writeln(f_pointeur_hexadecimal(m_pt_line));
}
        end; // with m_c_line

        if (m_buffer_index< m_buffer_size) and (m_oa_text_buffer[m_buffer_index]= k_return)
          then inc(m_buffer_index, 1);

        if (m_buffer_index< m_buffer_size) and (m_oa_text_buffer[m_buffer_index]= k_line_feed)
          then begin
              inc(m_buffer_index, 1);
              m_has_line_feed:= true;
            end
          else m_has_line_feed:= false;
      end; // f_read_line

    function c_text_buffer.f_test_line: Boolean;
      var l_save_buffer_index: Integer;
      begin
        l_save_buffer_index:= m_buffer_index;
        Result:= f_read_line;
        m_buffer_index:= l_save_buffer_index;
      end; // f_test_line

    function c_text_buffer.f_peek_line: String;
      var l_save_buffer_index: Integer;
      begin
        l_save_buffer_index:= m_buffer_index;

        while (m_buffer_index< m_buffer_size) and not (m_oa_text_buffer[m_buffer_index] in [k_return, k_line_feed]) do
          m_buffer_index:= m_buffer_index+ 1;

        Result:= f_extract_string_start_end(l_save_buffer_index, m_buffer_index- 1);

        m_buffer_index:= l_save_buffer_index;
      end; // f_peek_line

    function c_text_buffer.f_end_of_text: boolean;
      begin
        Result:= m_buffer_index>= m_buffer_size;
      end; // f_end_of_text

    procedure c_text_buffer.list;
      begin
        while not f_end_of_text do
        begin
          f_read_line;
          with m_c_line do
            display(Format('%5d >', [m_line_number])+ m_the_line+ '<');
        end; // while
      end; // list

    procedure c_text_buffer.list_strings(p_c_strings: tStrings);
      begin
        while not f_end_of_text do
        begin
          f_read_line;
          with m_c_line do
            p_c_strings.Add(Format('%5d >', [m_line_number])+ m_the_line+ '<');
        end; // while
      end; // list_strings

    procedure c_text_buffer.dump_hex(p_begin, p_end: Integer);
      begin
        if g_c_display= nil
          then display('*** no display strings')
          else
            // display_buffer_hex(Pointer(@ m_oa_text_buffer[p_begin]), p_end+ 1- p_begin, 16);
            display(f_display_hex(Pointer(@ m_oa_text_buffer[p_begin]), p_end+ 1- p_begin, 16, k_offset_hex_ascii));
      end; // dump_hex

    function c_text_buffer.f_extract_string_start_length(p_start, p_length: Integer): String;
      begin
        if p_length= 0
          then Result:= ''
          else begin
              if (p_length< 0) or (p_start+ p_length> m_buffer_size)
                then begin
                    stop('*** f_extract_string_start_length '
                        + m_name+ ' '
                        + IntToStr(p_start)+ ' '+ IntToStr(p_length)+ ' '+ IntToStr(m_buffer_size));
                    Result:= '';
                  end
                else begin
                    SetLength(Result, p_length);
                    Move(m_oa_text_buffer[p_start], Result[1], p_length);
                  end;
            end;
      end; // f_extract_string_start_length

    function c_text_buffer.f_extract_string_start_end(p_start, p_end: Integer): String;
        // -- [first_index..last_index]
      begin
        Result:= f_extract_string_start_length(p_start, p_end+ 1- p_start)
      end; // f_extract_string_start_end

    function c_text_buffer.f_extract_string_start_end_and_check(p_start, p_end: Integer;
        var pv_string: String): Boolean;
      begin
        if (p_start< 0) or (p_end>= m_buffer_size)
          then begin
              pv_string:= '';
              Result:= False;
            end
          else
            if p_end< p_start
              then begin
                  pv_string:= '';
                  Result:= True;
                end
              else begin
                  pv_string:= f_extract_string_start_end(p_start, p_end);
                  Result:= True;
                end;
      end; // f_extract_string_start_end_and_check

    function c_text_buffer.f_extract_string_start_end_max(p_start, p_end: Integer): String;
        // -- if not within bounds, do not yell (mainly used for debug display)
      begin
        if p_start< 0
          then p_start:= 0;
        if p_end>= m_buffer_size
          then p_end:= m_buffer_size- 1;

        if (p_start>= m_buffer_size)
           or
           (p_end< p_start)
          then begin
              Result:= '';
              Exit;
            end;

        Result:= f_extract_string_start_end(p_start, p_end);
      end; // f_extract_string_start_end_max

    // -- end of line

    function c_text_buffer.f_end_of_line(p_index: Integer): Boolean;
      begin
        if p_index>= m_buffer_size
          then begin
              display_bug_stop('c_text_buffer.f_end_of_line '+ IntToStr(p_index));
              Result:= True;
            end
          else result:= (m_oa_text_buffer[p_index]= k_return) or (m_oa_text_buffer[p_index]= k_line_feed);
      end; // f_end_of_line

    function c_text_buffer.f_end_of_line_size(p_index: Integer): Integer;
      begin
        if p_index= m_buffer_size- 1
          Then Result:= 1
          else begin
              if   (m_oa_text_buffer[p_index]= k_return) and (m_oa_text_buffer[p_index+ 1]= k_line_feed)
                 or
                   (m_oa_text_buffer[p_index]= k_line_feed) and (m_oa_text_buffer[p_index+ 1]= k_return)
                then Result:= 2
                else Result:= 1;
            end;    
      end; // f_end_of_line_size

    procedure c_text_buffer.skip_alternate_return_line_feed(var pv_index: Integer);
        // has found either RET or LF
        // if other, skip it
      var l_return_or_line_feed: Char;
      begin
        if pv_index>= m_buffer_size
          then begin
              display('*** c_text_buffer.skip_alternate_return_line_feed '+ IntToStr(pv_index));
              Halt;
            end;
        l_return_or_line_feed:= m_oa_text_buffer[pv_index];

        // -- skip this one
        inc(pv_index);

        if   (pv_index< m_buffer_size)
            and
              (
                  (l_return_or_line_feed= k_return) and (m_oa_text_buffer[pv_index]= k_line_feed)
                or
                  (l_return_or_line_feed= k_line_feed) and (m_oa_text_buffer[pv_index]= k_return)
              )
          then inc(pv_index);
      end; // skip_alternate_return_line_feed

    procedure c_text_buffer.skip_end_of_line(var pv_index: Integer);
      begin
        while (pv_index< m_buffer_size)
            and (m_oa_text_buffer[pv_index]<> k_line_feed)
            and (m_oa_text_buffer[pv_index]<> k_return) do
          inc(pv_index);

        skip_alternate_return_line_feed(pv_index);
      end; // skip_end_of_line

    function c_text_buffer.f_ascii_signature: Integer;
      var l_index: Integer;
      begin
        Result:= 0;
        l_index:= 0;
        while l_index< m_buffer_size do
        begin
          if not (m_oa_text_buffer[l_index]
              // in [k_return, k_line_feed, ' ', k_tabulation])
              in [k_return, k_line_feed, ' ', k_tabulation, '*', '{', '(', '}', ')'])
            then Inc(Result);
          Inc(l_index);
        end;
      end; // f_ascii_signature

    // -- replacement

    procedure c_text_buffer.replace_string(p_start_index: Integer; p_original, p_replacement: String);
      var l_original_length, l_replacement_length: Integer;
          l_delta: Integer;
          l_start_of_end, l_size_of_end: Integer;
      begin
        l_original_length:= Length(p_original);
        l_replacement_length:= Length(p_replacement);

        // -- make room for the new text, or shring if smaller
        l_delta:= l_replacement_length- l_original_length;
        l_start_of_end:= p_start_index+ l_original_length;
        l_size_of_end:= m_buffer_size- l_start_of_end;
        // display('replace at '+ IntToStr(p_start_index));
        if l_delta<> 0
          then begin
              move(m_oa_text_buffer[l_start_of_end], m_oa_text_buffer[l_start_of_end+ l_delta], l_size_of_end);
              Inc(m_buffer_size, l_delta);
            end;

        // -- now replace
        if l_replacement_length> 0
          then begin
              Move(p_replacement[1], m_oa_text_buffer[p_start_index], l_replacement_length);
            end;
      end; // replace_string

    function c_text_buffer.f_check_string(p_text_index: Integer; p_text: String): Boolean;
      var l_extract_text: String;
      begin
        l_extract_text:= f_extract_string_start_length(p_text_index, Length(p_text));
        Result:= l_extract_text= p_text
      end; // f_check_string

    function c_text_buffer.f_replace_string(p_start_index, p_original_length: Integer; p_replacement: String): Boolean;
      var l_replacement_length: Integer;
          l_delta: Integer;
          l_start_of_end, l_size_of_end: Integer;
      begin
        l_replacement_length:= Length(p_replacement);

        // -- make room for the new text, or shring if smaller
        l_delta:= l_replacement_length- p_original_length;
        l_start_of_end:= p_start_index+ p_original_length;
        l_size_of_end:= m_buffer_size- l_start_of_end;
        // display('replace at '+ IntToStr(p_start_index));
        if l_delta<> 0
          then begin
              move(m_oa_text_buffer[l_start_of_end], m_oa_text_buffer[l_start_of_end+ l_delta], l_size_of_end);
              Inc(m_buffer_size, l_delta);
            end;

        // -- now replace
        if l_replacement_length> 0
          then begin
              Move(p_replacement[1], m_oa_text_buffer[p_start_index], l_replacement_length);
            end;
        Result:= True;
      end; // f_replace_string

    function c_text_buffer.f_load_from_file(p_full_file_name: String): Boolean;
      begin
        Result:= f_load_oa_characters(p_full_file_name, m_oa_text_buffer);
        If Result
          then begin
              m_buffer_size:= Length(m_oa_text_buffer);
              m_buffer_index:= 0;
            end;
      end; // f_load_from_file

    procedure c_text_buffer.save_to_file(p_full_file_name: String);
      begin
        f_save_oa_characters(p_full_file_name, m_oa_text_buffer);
      end; // save_to_file

    // -- free

    destructor c_text_buffer.Destroy;
      begin
        // -- do NOT free it (if a descendent used a pointer to another open array)
        // -- Let Delphi do the reference counting and freeing
        // m_oa_text_buffer:= Nil;

        m_c_line.Free;
        m_c_line:= Nil;

        Inherited Destroy;
      end; // Destroy

    begin // c_text_buffer
    end. // c_text_buffer

