// 007 u_stringlist
// 03 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

(*$r+*)

unit u_stringlist;
  interface
    uses Classes
       , u_types_constants
       ;

    function f_c_add_stringlist(p_c_one, p_c_two: tStringList): tStringList;
    function f_c_substract_stringlist(p_c_one, p_c_two: tStringList): tStringList;
    function f_c_intersect_stringlist(p_c_one, p_c_two: tStringList): tStringList;
    function f_stringlist_to_list_of_strings(p_c_one: tStringList): String;
    function f_stringlist_to_separated_list(p_c_stringlist: tStringList; p_separator: String): String;
    procedure append_first_stringlist(p_c_one, p_c_two: tStringList);
    procedure append_second_stringlist(p_c_one, p_c_two: tStringList);
    procedure append_second_stringlist_unique(p_c_one, p_c_two: tStringList);
    procedure subtract_second_stringlist(p_c_one, p_c_two: tStringList);

    function f_display_stringlist_indented(p_indentation: Integer; p_c_stringlist: tStringList): String;
    procedure force_save_to_file(p_c_stringlist: tStringList; p_file_name: String);

    procedure free_and_nil_stringlist(var pv_c_stringlist: tStringlist);
    procedure load_strings_from_file(p_c_strings: tStrings; p_full_file_name: String);
    procedure swap_stringlists(var pv_c_one, pv_c_two: tStringList);

    procedure add_unique_name(p_c_strings: tStrings; p_name: String);
    procedure add_unique_name_if_not_nil(p_c_strings: tStrings; p_name: String);

    function f_are_equal(p_c_one, p_c_two: tStringList): Boolean;
    function f_one_includes_two(p_c_one, p_c_two: tStringList): Boolean;

    procedure load_strings_from_comma_separated_string(p_c_strings: tStrings; p_separated_string: String);
    procedure load_strings_from_blank_separated_string(p_c_strings: tStrings; p_separated_string: String);

    function f_c_create_and_load_stringlist(p_full_file_name: String): tStringList;
    procedure save_and_free_stringlist(var pv_c_stringlist: tStringList; p_full_file_name: String);
    // procedure save_to_file_and_free_stringlist(var pv_c_stringlist: tStringList; p_full_file_name: String);

    procedure display_delimited_stringlist(p_c_stringlist: tStrings; p_delimiter: Char);
    procedure display_stringlist_if_not_empty(p_title: String; p_c_stringlist: tStrings);

    procedure remove_blank_lines(p_c_stringlist: tStringList);
    procedure add_prefix(p_c_stringlist: tStringList; p_prefix: String);

    function f_length_max(p_c_strings: tStrings): Integer;

    // function f_pt_stringlist_integer_objects(p_c_stringlist: tStringList; p_list_index: Integer): t_pt_integer;
    procedure increment_stringlist_integer_objects(p_c_stringlist: tStringList; p_list_index: Integer);
    function f_get_stringlist_integer_objects(p_c_stringlist: tStringList; p_list_index: Integer): Integer;

    procedure remove_duplicates(p_c_stringlist: tStringList);

    procedure free_and_create_stringlist(var pv_c_stringlist: tStringList);
    function f_display_separated_list(p_c_string_list: tStringList; p_separator: Char): String;
    function f_is_quoted_string_list(p_c_string_list: tStringList): Boolean;
    function f_display_quoted_list(p_c_string_list: tStringList): String;
    function f_display_separated_quoted_list(p_c_string_list: tStringList; p_separator: Char): String;

  implementation
    uses SysUtils, FileCtrl, Math
        , u_characters
        , u_c_display
        , u_file
        , u_strings;

    function f_c_add_stringlist(p_c_one, p_c_two: tStringList): tStringList;
      var l_string_index: Integer;
      begin
        Result:= tStringList.Create;
        Result.Assign(p_c_one);
        for l_string_index:= 0 to p_c_two.Count- 1 do
          Result.Add(p_c_two[l_string_index]);
      end; // f_c_add_stringlist

    procedure append_first_stringlist(p_c_one, p_c_two: tStringList);
      var l_string_index: Integer;
      begin
        for l_string_index:= 0 to p_c_one.Count- 1 do
          p_c_two.Add(p_c_one[l_string_index]);
      end; // append_first_stringlist

    procedure append_second_stringlist(p_c_one, p_c_two: tStringList);
      var l_string_index: Integer;
      begin
        for l_string_index:= 0 to p_c_two.Count- 1 do
          p_c_one.Add(p_c_two[l_string_index]);
      end; // append_second_stringlist

    procedure append_second_stringlist_unique(p_c_one, p_c_two: tStringList);
      var l_string_index: Integer;
          l_string: String;
      begin
        for l_string_index:= 0 to p_c_two.Count- 1 do
        begin
          l_string:= p_c_two[l_string_index];
          if p_c_one.IndexOf(l_string)< 0
            then p_c_one.Add(l_string);
        end; // for l_string_index    
      end; // append_second_string_list_unique

    function f_c_substract_stringlist(p_c_one, p_c_two: tStringList): tStringList;
      var l_string_index, l_index_of: Integer;
      begin
        // display_strings('one', p_c_one);
        // display_strings('two', p_c_two);
        Result:= tStringList.Create;
        Result.Assign(p_c_one);

        with p_c_two do
          for l_string_index:= 0 to Count- 1 do
          begin
            l_index_of:= Result.IndexOf(Strings[l_string_index]);
            if l_index_of>= 0
              then begin
                  // display('  delete '+ Strings[l_string_index]);
                  Result.Delete(l_index_of);
                end;
          end; // with, for

        // display_strings('one- two', Result);
      end; // f_c_substract_stringlist

    procedure subtract_second_stringlist(p_c_one, p_c_two: tStringList);
        // -- removes from ONE the elemens NOT in two
      var l_string_index, l_index_of: Integer;
      begin
        with p_c_two do
          for l_string_index:= 0 to Count- 1 do
          begin
            l_index_of:= p_c_one.IndexOf(Strings[l_string_index]);
            if l_index_of>= 0
              then begin
                  // display('  delete '+ Strings[l_string_index]);
                  p_c_one.Delete(l_index_of);
                end;
          end; // with, for
      end; // subtract_second_stringlist

    function f_c_intersect_stringlist(p_c_one, p_c_two: tStringList): tStringList;
      var l_string_index: Integer;
      begin
        Result:= tStringList.Create;

        with p_c_one do
          for l_string_index:= 0 to Count- 1 do
            if p_c_two.IndexOf(Strings[l_string_index])>= 0
              then Result.Add(Strings[l_string_index]);

        with p_c_two do
          for l_string_index:= 0 to Count- 1 do
            if (p_c_one.IndexOf(Strings[l_string_index])>= 0) and (Result.IndexOf(Strings[l_string_index])< 0)
              then Result.Add(Strings[l_string_index]);

        // display_strings('one- two', Result);
      end; // f_intersect_stringlist

        
(*
    function f_c_create_string_set(p_name: String; p_c_string_list: tStringList): c_string_set;
      begin
        Result:= c_string_set.create_string_set(p_name);
        Result.add_set(p_c_string_list);
      end; // f_c_create_string_set
*)
    function f_stringlist_to_list_of_strings(p_c_one: tStringList): String;
      var l_string_index: Integer;
      begin
        Result:= '';
        for l_string_index:= 0 to p_c_one.Count- 1 do
        begin
          Result:= Result+ p_c_one[l_string_index];
          if l_string_index< p_c_one.Count- 1
            then Result:= Result+ ', ';
        end;
      end; // f_stringlist_to_list_of_strings

    function f_stringlist_to_separated_list(p_c_stringlist: tStringList; p_separator: String): String;
      var l_string_index: Integer;
      begin
        Result:= '';
        for l_string_index:= 0 to p_c_stringlist.Count- 1 do
        begin
          Result:= Result+ p_c_stringlist[l_string_index];
          if l_string_index< p_c_stringlist.Count- 1
            then Result:= Result+ p_separator;
        end;
      end; // f_stringlist_to_separated_list

    function f_display_stringlist_indented(p_indentation: Integer; p_c_stringlist: tStringList): String;
      var l_index: Integer;
      begin
        Result:= '';
        with p_c_stringlist do
          for l_index:= 0 to Count- 1 do
          begin
            Result:= Result+ f_spaces(p_indentation)+ Strings[l_index];
            if l_index< Count- 1
              then Result:= Result+ k_new_line;
          end;
      end; // f_display_stringlist_indented

    procedure force_save_to_file(p_c_stringlist: tStringList; p_file_name: String);
      var l_path: String;
      begin
        l_path:= ExtractFilePath(p_file_name);
        if Not DirectoryExists(l_path)
          then ForceDirectories(l_path);
        p_c_stringlist.SaveToFile(p_file_name);
      end; // force_save_to_file

    procedure free_and_nil_stringlist(var pv_c_stringlist: tStringlist);
      begin
        pv_c_stringlist.Free;
        pv_c_stringlist:= Nil;
      end; // free_and_nil_stringlist

    procedure load_strings_from_file(p_c_strings: tStrings; p_full_file_name: String);
      begin
        if not FileExists(p_full_file_name)
          then display_bug_stop('no '+ p_full_file_name);
        p_c_strings.LoadFromFile(p_full_file_name);
      end; // load_strings_from_file

    procedure swap_stringlists(var pv_c_one, pv_c_two: tStringList);
      var l_c_temporary_stringlist: tStringList;
      begin
        l_c_temporary_stringlist:= pv_c_one;
        pv_c_one:= pv_c_two;
        pv_c_two:= l_c_temporary_stringlist;
      end; // swap_stringlists

    procedure add_unique_name(p_c_strings: tStrings; p_name: String);
      begin
        with p_c_strings do
          if IndexOf(p_name)< 0
            then Add(p_name);
      end; // add_unique_name

    procedure add_unique_name_if_not_nil(p_c_strings: tStrings; p_name: String);
      begin
        if p_c_strings<> Nil
          then
            with p_c_strings do
              if IndexOf(p_name)< 0
                then Add(p_name);
      end; // add_unique_name_if_not_nil

    function f_are_equal(p_c_one, p_c_two: tStringList): Boolean;
        // -- assumes that have no duplicates
      var l_string_index: Integer;
      begin
        Result:= p_c_one.Count= p_c_two.Count;

        if Result
          then
            for l_string_index:= 0 to p_c_one.Count- 1 do
              if p_c_one.Strings[l_string_index]<> p_c_two.Strings[l_string_index]
                then begin
                    Result:= False;
                    Break;
                  end;
      end; // f_are_equal

    function f_one_includes_two(p_c_one, p_c_two: tStringList): Boolean;
        // -- all elements of two are in one
        // -- False if at least 1 element of TWO is not in ONE
      var l_string_index: Integer;
      begin
        Result:= True;

        for l_string_index:= 0 to p_c_two.Count- 1 do
          if p_c_one.IndexOf(p_c_two.Strings[l_string_index])< 0
            then begin
                Result:= False;
                Break;
              end;
      end; // f_one_includes_two

    // -- file

    procedure load_strings_from_comma_separated_string(p_c_strings: tStrings; p_separated_string: String);
      var l_index, l_length: Integer;
          l_word: String;
      begin
        l_index:= 1; l_length:= Length(p_separated_string);
        while l_index< l_length do
        begin
          l_word:= f_string_extract_comma_separated(p_separated_string, l_index);
          if l_word<> ''
            then p_c_strings.Add(l_word);
        end; // while l_index
      end; // load_strings_from_comma_separated_string

    procedure load_strings_from_blank_separated_string(p_c_strings: tStrings; p_separated_string: String);
      var l_index, l_length: Integer;
          l_word: String;
      begin
        l_index:= 1; l_length:= Length(p_separated_string);
        while l_index< l_length do
        begin
          l_word:= f_string_extract_non_blank(p_separated_string, l_index);
          if l_word<> ''
            then p_c_strings.Add(l_word);
        end; // while l_index
      end; // load_strings_from_blank_separated_string

    function f_c_create_and_load_stringlist(p_full_file_name: String): tStringList;
      begin
        if Not FileExists(p_full_file_name)
          then begin
              check_path_and_name(p_full_file_name);
              Result:= Nil;
              display_bug_halt('no '+ p_full_file_name);
            end
          else begin
              Result:= tStringList.Create;
              Result.LoadFromFile(p_full_file_name);
            end;
      end; // f_c_create_and_load_stringlist

    procedure display_delimited_stringlist(p_c_stringlist: tStrings; p_delimiter: Char);
      var l_list_index: Integer;
      begin
        with p_c_stringlist do
          for l_list_index:= 0 to Count- 1 do
            display(p_delimiter+ Strings[l_list_index]+ p_delimiter);
      end; // display_delimited_stringlist

    procedure display_stringlist_if_not_empty(p_title: String; p_c_stringlist: tStrings);
      var l_list_index: Integer;
      begin
        with p_c_stringlist do
          if Count> 0
            then begin
                display(p_title);
                for l_list_index:= 0 to Count- 1 do
                  display(Strings[l_list_index]);
              end;
      end; // display_stringlist_if_not_empty

    procedure save_and_free_stringlist(var pv_c_stringlist: tStringList; p_full_file_name: String);
      begin
        pv_c_stringlist.SaveToFile(p_full_file_name);
        pv_c_stringlist.Free;
        pv_c_stringlist:= Nil;
      end; // save_and_free_stringlist

    procedure remove_blank_lines(p_c_stringlist: tStringList);
      var l_list_index: Integer;
      begin
        with p_c_stringlist do
        begin
          l_list_index:= 0;
          while l_list_index< Count do
            if Trim(Strings[l_list_index])= ''
              then Delete(l_list_index)
              else Inc(l_list_index);
        end; // with p_c_stringlist
      end; // remove_blank_lines

    procedure add_prefix(p_c_stringlist: tStringList; p_prefix: String);
        // -- add the prefix to all lines
      var l_list_index: Integer;
      begin
        with p_c_stringlist do
          for l_list_index:= 0 to Count- 1 do
            Strings[l_list_index]:= p_prefix+ Strings[l_list_index];
      end; // add_prefix

    function f_length_max(p_c_strings: tStrings): Integer;
      var l_list_index: Integer;
      begin
        Result:= 0;
        with p_c_strings do
          for l_list_index:= 0 to Count- 1 do
            Result:= Max(Result, Length(Strings[l_list_index]));
      end; // f_length_max

(*
    function f_pt_stringlist_integer_objects(p_c_stringlist: tStringList; p_list_index: Integer): t_pt_integer;
      begin
        display('access');
        Result:= t_pt_integer(Integer(@ p_c_stringlist.Objects[p_list_index]));
      end; // f_pt_stringlist_integer_objects
*)

    procedure increment_stringlist_integer_objects(p_c_stringlist: tStringList; p_list_index: Integer);
      begin
        with p_c_stringlist do
          Objects[p_list_index]:= tObject(Integer(Objects[p_list_index])+ 1);
      end; // increment_stringlist_integer_objects

    function f_get_stringlist_integer_objects(p_c_stringlist: tStringList; p_list_index: Integer): Integer;
      begin
        Result:= Integer(p_c_stringlist.Objects[p_list_index]);
      end; // f_get_stringlist_integer_objects

    procedure remove_duplicates(p_c_stringlist: tStringList);
      var l_list_index: Integer;
          l_string: String;
          l_search_index: Integer;
      begin
        l_list_index:= 0;

        // display_strings('list', p_c_stringlist);

        while l_list_index< p_c_stringlist.Count do
        begin
          l_string:= p_c_stringlist[l_list_index];
          // display(l_string);
          l_search_index:= l_list_index+ 1;
          while l_search_index< p_c_stringlist.Count do
            if p_c_stringlist[l_search_index]= l_string
              then begin
                  // display('strl_del '+ l_string);
                  p_c_stringlist.Delete(l_search_index);
                end
              else Inc(l_search_index);

          Inc(l_list_index);
        end;
      end; // remove_duplicates
      
    procedure free_and_create_stringlist(var pv_c_stringlist: tStringList);
      begin
        pv_c_stringlist.Free;
        pv_c_stringlist:= tStringList.Create;
      end; // free_and_create_stringlist

    function f_display_separated_list(p_c_string_list: tStringList; p_separator: Char): String;
      var l_list_index: Integer;
      begin
        result:= '';
        with p_c_string_list do
          for l_list_index:= 0 to Count- 1 do
          begin
            Result:= Result+ Strings[l_list_index];
            if l_list_index< Count- 1
              then Result:= Result+ p_separator;
          end;    
      end; // f_display_separated_list

    function f_is_quoted_string_list(p_c_string_list: tStringList): Boolean;
      var l_list_index: Integer;
          l_string: String;
      begin
        Result:= True;

        with p_c_string_list do
          for l_list_index:= 0 to Count- 1 do
          begin
            l_string:= Strings[l_list_index];
            if (Length(l_string)> 1) and (l_string[1]= '''') and (l_string[Length(l_string)]= '''')
              then
              else begin
                  Result:= False;
                  Break;
                end;
          end;
      end; // f_is_quoted_string_list

    function f_display_quoted_list(p_c_string_list: tStringList): String;
      var l_list_index: Integer;
          l_string: String;
      begin
        result:= '';
        with p_c_string_list do
          for l_list_index:= 0 to Count- 1 do
          begin
            l_string:= Strings[l_list_index];
            System.Delete(l_string, 1, 1);
            System.Delete(l_string, Length(l_string), 1);
            Result:= Result+ l_string;
          end;
      end; // f_display_quoted_list

    function f_display_separated_quoted_list(p_c_string_list: tStringList; p_separator: Char): String;
      var l_list_index: Integer;
          l_string: String;
      begin
        result:= '';
        with p_c_string_list do
          for l_list_index:= 0 to Count- 1 do
          begin
            l_string:= Strings[l_list_index];
            System.Delete(l_string, 1, 1);
            System.Delete(l_string, Length(l_string), 1);
            Result:= Result+ l_string;
            if (l_list_index< Count- 1) and (p_separator<> chr(0))
              then Result:= Result+ p_separator;
          end;
      end; // f_display_separated_quoted_list

    begin // u_stringlist
    end. // u_stringlist
