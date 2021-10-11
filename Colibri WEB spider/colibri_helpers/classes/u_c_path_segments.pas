// 002 u_c_path_segments
// 06 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

// -- handle the parts of a path
// -- builds a tStringList of the segments
// -- if there is a drive letter, place it as the first segment

(*$r+*)

unit u_c_path_segments;
  interface
    uses Classes, u_c_basic_object;

    type c_path_segments= class(c_basic_object)
                            // -- m_name: anything
                            m_path: String;
                            m_c_segments: tStringList;
                            m_has_drive: Boolean;

                            Constructor create_path_segments(p_name, p_path: String);
                            function f_path_segment_count: Integer;
                            function f_path_segment(p_index: Integer): String;
                            function f_absolute_path(p_relative_path: String): String;
                            function f_absolute_path_2(p_relative_path: String): String;

                            function f_display_path_segments_detail: String;
                            function f_complete_path: String;

                            function f_common_path_with(p_c_path_segments: c_path_segments): String;
                            function f_without_common_path_with(p_c_path_segments: c_path_segments): String;
                            function f_indent_different_path_from(p_c_path_segments: c_path_segments): String;

                            Destructor Destroy; Override;
                          end; // c_path_segments

    function f_common_path(pk_one, pk_two: String): String;
    function f_last_path_segment(p_path: String): String;
    function f_remove_middle_dot_dot(p_path: String): String;
    function f_relative_path_with_display(p_source_path, p_target_path: String; p_display: Boolean): String;
    function f_relative_path(p_source_path, p_target_path: String): String;
    function f_remove_middle_dot(p_path: String): String;
    function f_replace_last_segment(p_path, p_new_last_segment: String): String;

  implementation
    uses SysUtils, u_c_display
        , u_dir
        , u_strings
        ;

    // c_path_segments

    Constructor c_path_segments.create_path_segments(p_name, p_path: String);

      procedure build_segments;
        var l_index, l_start_index: Integer;
            l_segment_length: Integer;
            l_segment: String;
        begin
          if (Length(m_path)> 2) and (m_path[2]= ':') and (m_path[3]= '\')
            then begin
                m_c_segments.Add(m_path[1]+ ':');
                l_index:= 4;
                m_has_drive:= True;
              end
            else l_index:= 1;

          while l_index<= Length(m_path) do
          begin
            // -- sikp \
            while (l_index<= Length(m_path)) and (m_path[l_index]= '\') do
              Inc(l_index);

            // -- now get the segment
            l_start_index:= l_index;
            while (l_index<= Length(m_path)) and (m_path[l_index]<> '\') do
              Inc(l_index);

            l_segment_length:= l_index- l_start_index;
            if l_segment_length> 0
              then begin
                  SetLength(l_segment, l_segment_length);
                  Move(m_path[l_start_index], l_segment[1], l_segment_length);
                  m_c_segments.Add(l_segment);
                end
          end; // while l_index
        end; // build_segments

      begin // create_path_segments
        Inherited create_basic_object(p_name);

        m_c_segments:= tStringList.Create;
        m_path:= p_path;

        build_segments;
      end; // create_path_segments

    function c_path_segments.f_path_segment_count: Integer;
      begin
        if m_c_segments= Nil
          then display_bug_halt('c_path_segments NIL');
        Result:= m_c_segments.Count;
      end; // f_path_segment_count

    function c_path_segments.f_path_segment(p_index: Integer): String;
      begin
        Result:= m_c_segments[p_index];
      end; // f_path_segment

    function c_path_segments.f_display_path_segments_detail: String;
        // -- debug
      var l_segment_index: Integer;
      begin
        Result:= '';
        with m_c_segments do
          for l_segment_index:= 0 to f_path_segment_count- 1 do
            if l_segment_index< f_path_segment_count- 1
              then Result:= Result+ f_path_segment(l_segment_index)+ ' | '
              else Result:= Result+ f_path_segment(l_segment_index);
      end; // f_display_path_segments_detail

    function c_path_segments.f_absolute_path(p_relative_path: String): String;
        // -- for project copier: .DOF contains now relative, and has to compute absolue
        // -- ex
        // -- c:\programs\colibri\utilities
        // --                              \exe
        // --                              \web\pascal_to_html
        // --   DOF: output is "..\..\exe
        // --   wants "c:\programs\colibri\utilities\exe"
      const k_display= False;
      var l_length, l_index: Integer;
          l_level: Integer;
          l_relative_path: String;
      begin
        l_length:= Length(p_relative_path);
        // -- start at max level (project)
        l_level:= f_path_segment_count- 1;
        // -- back-up for each ..\ group
        l_index:= 1;
        l_relative_path:= p_relative_path;
        while (l_index+ 3<= l_length) and (copy(l_relative_path, 1, 3)= '..\') do
        begin
          Delete(l_relative_path, 1, 3);
          Dec(l_level);
          Inc(l_index, 3);
        end;

        // -- not a relative path starting with ..\
        if l_index= 1
          then begin
              Result:= p_relative_path;
              Exit;
            end;

        // -- now build the path from the root
        Result:= '';
        for l_index:= 0 to l_level do
          Result:= Result+ f_path_segment(l_index)+ '\';

        if k_display
          then display(IntToStr(l_level)+ ' '+ Result);

        // -- and add the final part
        Result:= Result+ l_relative_path;

        if k_display
          then begin
              display('  '+ p_relative_path);
              display('    '+ Result);
            end;
      end; // f_absolute_path

    function c_path_segments.f_absolute_path_2(p_relative_path: String): String;
        // -- for project copier: .DOF contains now relative, and has to compute absolue
        // -- ex
        // -- c:\programs\colibri\utilities
        // --                              \exe
        // --                              \web\pascal_to_html
        // --   DOF: output is "..\..\exe
        // --   wants "c:\programs\colibri\utilities\exe"
        // -- ded 2004: correct bug: if "\xxx" or "xxx" it is a simple sub path
      const k_display= False;
      var l_length, l_index: Integer;
          l_level: Integer;
          l_relative_path: String;
      begin
        l_length:= Length(p_relative_path);
        if l_length= 0
          then begin
              Result:= '';
              Exit;
            end;

        if Pos('..', p_relative_path)<= 0
          then begin
              // display('NO_REL '+ p_relative_path+ ' '+ p_relative_path[1]);
              // -- m_path has already ending \
              if p_relative_path[1]= '\'
                then begin
                    Result:= p_relative_path;
                    Delete(Result, 1, 1);
                    Result:= m_path+ Result;
                  end
                else Result:= m_path+ p_relative_path;
              Exit;
            end;

        // -- start at max level (project)
        l_level:= f_path_segment_count- 1;
        // -- back-up for each ..\ group
        l_index:= 1;
        l_relative_path:= p_relative_path;
        while (l_index+ 3<= l_length) and (copy(l_relative_path, 1, 3)= '..\') do
        begin
          Delete(l_relative_path, 1, 3);
          Dec(l_level);
          Inc(l_index, 3);
        end;

        // -- not a relative path starting with ..\
        if l_index= 1
          then begin
              Result:= p_relative_path;
              Exit;
            end;

        // -- now build the path from the root
        Result:= '';
        for l_index:= 0 to l_level do
          Result:= Result+ f_path_segment(l_index)+ '\';

        if k_display
          then display(IntToStr(l_level)+ ' '+ Result);

        // -- and add the final part
        Result:= Result+ l_relative_path;

        if k_display
          then begin
              display('  '+ p_relative_path);
              display('    '+ Result);
            end;
      end; // f_absolute_path_2

    function c_path_segments.f_complete_path: String;
        // -- rebuild the whole thing
      var l_segment_index: Integer;
      begin
        if m_has_drive
          then begin
              l_segment_index:= 1;
              Result:= f_path_segment(0)+ '\'
            end
          else begin
              l_segment_index:= 0;
              Result:= '';
            end;

        while l_segment_index< f_path_segment_count do
        begin
          Result:= Result+ f_path_segment(l_segment_index)+ '\';
          Inc(l_segment_index);
        end;
      end; // f_complete_path

    // -- common segment computations

    function c_path_segments.f_common_path_with(p_c_path_segments: c_path_segments): String;
        // -- return the common prefix of 2 pathes
      var l_index: Integer;
      begin
        Result:= '';

        // -- ? start at 1 (after the drive ?)
        l_index:= 0;

        while (l_index< f_path_segment_count)
            and (l_index< p_c_path_segments.f_path_segment_count)
            and (f_path_segment(l_index)= p_c_path_segments.f_path_segment(l_index)) do
        begin
          Result:= Result+ f_path_segment(l_index)+ '\';
          Inc(l_index);
        end; // while common
      end; // f_common_path_with

    function c_path_segments.f_without_common_path_with(p_c_path_segments: c_path_segments): String;
        // -- return "1 - common_prefix_with_2"
      var l_common_path_with: String;
      begin
        l_common_path_with:= f_common_path_with(p_c_path_segments);

        Result:= f_remove_start_if_start_is_equal_to(m_path, l_common_path_with)
      end; // f_without_common_path_with

    function c_path_segments.f_indent_different_path_from(p_c_path_segments: c_path_segments): String;
        // -- return "---- 1 - common_prefix_with_2"
      var l_common_path_with: String;
      begin
        l_common_path_with:= f_common_path_with(p_c_path_segments);
        // display('co %'+ l_common_path_with+ '%'); 
        Result:= f_spaces(Length(l_common_path_with))+ f_without_common_path_with(p_c_path_segments);
      end; // f_indent_different_path_from

    Destructor c_path_segments.Destroy;
      begin
        m_c_segments.Free;

        Inherited;
      end; // Destroy

    // -- global procedures

    function f_common_path(pk_one, pk_two: String): String;
      var l_c_one, l_c_two: c_path_segments;
      var l_index: Integer;
      begin
        l_c_one:= c_path_segments.create_path_segments('', pk_one);
        l_c_two:= c_path_segments.create_path_segments('', pk_two);

        Result:= '';
        l_index:= 1;

        while (l_index< l_c_one.f_path_segment_count)
            and (l_index< l_c_two.f_path_segment_count)
            and (l_c_one.f_path_segment(l_index)= l_c_two.f_path_segment(l_index)) do
        begin
          result:= Result+ l_c_one.f_path_segment(l_index)+ '\';
          Inc(l_index);
        end;

        l_c_one.Free;
        l_c_two.Free;
      end; // f_common_path

    function f_remove_middle_dot_dot(p_path: String): String;
        // a\b\..\c    =>   a\c
      const k_display= False;
      var l_segment: Integer;
          l_consecutive_dot_dot: Integer;
          l_first_segment_index: Integer;
          // l_textual_segment: Integer;
      begin
        if k_display
          then display(p_path);
(*
        if Pos('..\', p_path)<= 0
          then begin
              Result:= p_path;
              Exit;
            end;
*)
        with c_path_segments.create_path_segments('', p_path) do
        begin
          if f_path_segment_count<= 1
            then begin
                Result:= p_path;
                Exit;
              end;

          if m_has_drive
            then l_first_segment_index:= 1
            else l_first_segment_index:= 0;

          Result:= '';
          l_segment:= f_path_segment_count- 1;
          l_consecutive_dot_dot:= 0;

          // -- start from the end
          // --   if detects some ..\, count them, and remove the
          // --   same amount of preceeding non-dot segments
          while l_segment>= l_first_segment_index do
          begin
            if f_path_segment(l_segment)= '..'
              then begin
                  if k_display
                    then display('  '+ IntToStr(l_segment)+ ' dot');
                  Inc(l_consecutive_dot_dot);
                  Dec(l_segment);
                end
              else begin
                  if k_display
                    then display('  '+ IntToStr(l_segment)+ ' seg '+ f_path_segment(l_segment)+ ' '+ IntToStr(l_consecutive_dot_dot));
                  // -- skip the backed up segments
                  if l_consecutive_dot_dot> 0
                    then begin
                        Dec(l_consecutive_dot_dot);
                        Dec(l_segment);
                        if k_display
                          then display('    rem');
                      end
                    else begin
                        if k_display
                          then display('    add');
                        Result:= f_path_segment(l_segment)+ '\'+ Result;
                        Dec(l_segment);
                      end
                end;
          end;

          while l_consecutive_dot_dot> 0 do
          begin
            Result:= '..\'+ Result;
            Dec(l_consecutive_dot_dot);
          end;

          if l_first_segment_index> 0
            then Result:= f_path_segment(0)+ '\'+ Result;

          Free;
        end;
      end; // f_remove_middle_dot_dot

    function f_last_path_segment(p_path: String): String;
      begin
        with c_path_segments.create_path_segments('', p_path) do
        begin
          // stop(m_path);
          if f_path_segment_count> 0
            then Result:= f_path_segment(f_path_segment_count- 1)
            else Result:= '';
          Free;
        end;
      end; // f_last_path_segment

    function f_relative_path_with_display(p_source_path, p_target_path: String; p_display: Boolean): String;
        // -- compute helper pathes for DOF
        // --   "c:\prog\web\mail" + "c:\prog\helper\classes\"
        // --   => "..\..\helpers\classes\"
        // -- both MUST containt drive
        // --
        // -- the target could be relative (path local to the dpr)
      var l_c_source, l_c_target: c_path_segments;
          l_index: Integer;
          l_back_up_source_index, l_forward_target_index: Integer;
          // -- debug
          l_display_common, l_display_up, l_display_down: String;
      begin
        if p_display
          then begin
              display('rel');
              display('  source '+ p_source_path);
              display('  target '+ p_target_path);
            end;

        if f_contains_dot_dot(p_source_path)
            or not f_contains_drive(p_source_path)
            or f_contains_dot_dot(p_target_path)
            or not f_contains_drive(p_target_path)
          then begin
              Result:= p_target_path;
              if p_display
                then display('  no_drive_or_rel');
              Exit;
            end;

        l_c_source:= c_path_segments.create_path_segments('', p_source_path);
        l_c_target:= c_path_segments.create_path_segments('', p_target_path);

        // -- start after the drive
        l_index:= 1;

        // -- find the common part
        l_display_common:= '';
        while (l_index< l_c_source.f_path_segment_count)
            and (l_index< l_c_target.f_path_segment_count)
            and (l_c_source.f_path_segment(l_index)= l_c_target.f_path_segment(l_index)) do
        begin
          l_display_common:= l_display_common+ l_c_source.f_path_segment(l_index)+ '\';
          Inc(l_index);
        end;

        if p_display
          then display('  com      '+ l_display_common);

        // -- set index to last common segment
        Dec(l_index);

        if l_index> 0
          then begin
              l_display_up:= '';
              for l_back_up_source_index:= l_index+ 1 to l_c_source.f_path_segment_count- 1 do
                l_display_up:= l_display_up+ '..\';

              l_display_down:= '';
              for l_forward_target_index:= l_index+ 1 to l_c_target.f_path_segment_count- 1 do
                l_display_down:= l_display_down+ l_c_target.f_path_segment(l_forward_target_index)+ '\';

              if p_display
                then begin
                    display('  up       '+ l_display_up);
                    display('  down     '+ l_display_down);
                  end;
               Result:= l_display_up+ l_display_down;
            end
          else Result:= p_target_path;

        l_c_source.Free;
        l_c_target.Free;
      end; // f_relative_path

    function f_relative_path(p_source_path, p_target_path: String): String;
      begin
        Result:= f_relative_path_with_display(p_source_path, p_target_path, False);
      end; // f_relative_path

    function f_remove_middle_dot(p_path: String): String;
        // -- "." is the "self" directory
        // -- "a\b\.\c"    =>   "a\b\c"
        // -- if no drive, always
        // --  - remove starting \
        // --  - with ONE ending slash
      const k_display= False;
      var l_segment_index: Integer;
      begin
        if k_display
          then display(p_path);

        with c_path_segments.create_path_segments('', p_path) do
        begin
          if f_path_segment_count<= 1
            then begin
                Result:= p_path;
                Exit;
              end;

          if m_has_drive
            then begin
                l_segment_index:= 1;
                Result:= f_path_segment(0)+ '\'
              end
            else begin
                l_segment_index:= 0;
                Result:= '';
              end;

          // -- all . are removed
          while l_segment_index< f_path_segment_count do
          begin
            if f_path_segment(l_segment_index)= '.'
              then begin
                  if k_display
                    then display('  '+ IntToStr(l_segment_index)+ ' dot');
                  Inc(l_segment_index);
                end
              else begin
                  if k_display
                    then display('  '+ IntToStr(l_segment_index)+ ' seg '+ f_path_segment(l_segment_index));
                  Result:= Result+ f_path_segment(l_segment_index)+ '\';
                  Inc(l_segment_index);
                end;
          end; // while

          Free;
        end; // with c_path_segments
      end; // f_remove_middle_dot

    function f_replace_last_segment(p_path, p_new_last_segment: String): String;
      begin
        with c_path_segments.create_path_segments('', p_path) do
        begin
          if f_path_segment_count> 0
            then begin
                m_c_segments[f_path_segment_count- 1]:= f_without_ending_slash(p_new_last_segment);
                Result:= f_complete_path;
              end
            else Result:= p_path;
          Free;
        end;
      end; // f_replace_last_segment

    begin // u_c_path_segments
    end. // u_c_path_segments


procedure Tform_classes.Button1Click(Sender: TObject);
begin
  display('  '+ f_remove_middle_dot_dot('a\..\b\text.txt'));
  display('  '+ f_remove_middle_dot_dot('..\b\text.txt'));
  display('  '+ f_remove_middle_dot_dot('C:\..\b\text.txt'));
  display('  '+ f_remove_middle_dot_dot('a\b\..\b\..\..\text.txt'));
end;




    procedure TForm1.trial_Click(Sender: TObject);
      // -- test remove single .

      procedure remove(p_text: String);
        begin
          display('seg '+ p_text+ ' => '+ f_remove_middle_dot(p_text)+ '<');
        end;

      begin
        remove('a\.\b');
        remove('a\.\b\');
        remove('\a\.\b\');
        remove('\a\\\.\b\');
        remove('c:\a\\\.\b\');
        remove('http:\\a\\\.\b\');
        remove('\a\.\.\.\b\');
        remove('.\b\');
        remove('.\.\.\b\');
        remove('a\.\b\.');
        remove('a\.\b\.\');
        remove('a\.\b\.\.\');
      end;

