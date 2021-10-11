// 002 u_c_log_list
// 07 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

// -- one class for several log files
// --   allows a single creation, and consistent simple call to add to a log

// -- log_list_name: the path of all logs
// -- each log: the individual file name

// -- CAUTION: there was a global g_c_log_list

// -- documentation at http://www.jcolibri.com


(*
      add_unique_log('xxx');
      write_line('xxx', 'zzz');
*)

(*$r+*)

unit u_c_log_list;
  interface
    uses Classes, u_c_basic_object, u_c_log;

    type c_log_list= // "log" list
                     Class(c_basic_object)
                       public
                         // -- m_name: the base file name: 'c:\prog\exe\log\log_uses'
                         m_c_log_list: tStringList;

                         Constructor create_log_list(p_name: String); Virtual;

                         function f_log_count: Integer;

                         function f_c_log(p_log_index: Integer): c_log;
                         function f_index_of(p_log: String): Integer;
                         function f_find_by_c_log(p_log: String): c_log;

                         procedure add_log(p_log: String; p_c_log: c_log);
                         procedure add_unique_log(p_log: String);

                         procedure write_string(p_log, p_text: String);
                         procedure write_line(p_log, p_text: String);

                         procedure display_log_list;

                         Destructor Destroy; Override;
                       end;

  // var g_c_log_list: c_log_list= Nil;

  implementation
    uses SysUtils, u_c_display;

    // -- c_log_list

    Constructor c_log_list.create_log_list(p_name: String);
      begin
        Inherited create_basic_object(p_name);

        m_c_log_list:= tStringList.Create;
      end; // create_log_list

    function c_log_list.f_log_count: Integer;
      begin
        Result:= m_c_log_list.Count;
      end; // f_log_count

    function c_log_list.f_c_log(p_log_index: Integer): c_log;
      begin
        Result:= c_log(m_c_log_list.Objects[p_log_index]);
      end; //  f_log

    function c_log_list.f_index_of(p_log: String): Integer;
      begin
        Result:= m_c_log_list.IndexOf(p_log);
      end; // f_index_of

    function c_log_list.f_find_by_c_log(p_log: String): c_log;
      var l_index_of: Integer;
      begin
        l_index_of:= f_index_of(p_log);
        if l_index_of< 0
          then Result:= Nil
          else Result:= c_log(m_c_log_list.Objects[l_index_of]);
      end; // f_find_by_c_log

    procedure c_log_list.add_log(p_log: String; p_c_log: c_log);
      begin
        m_c_log_list.AddObject(p_log, tObject(p_c_log));
      end; // add_log

    procedure c_log_list.add_unique_log(p_log: String);
      var l_c_log: c_log;
      begin
        if f_index_of(p_log)>= 0
          then display_bug_halt('already this log '+ p_log)
          else begin
              // display('create_log '+ m_name+ p_log+ '.txt');
              l_c_log:= c_log.create_log('log', m_name+ p_log+ '.txt');
              // l_c_log.create_and_close;
              add_log(p_log, l_c_log);
            end;
      end; // add_unique_log

    procedure c_log_list.display_log_list;
      var l_log: Integer;
      begin
        display(m_name+ ' '+ IntToStr(f_log_count));

        for l_log:= 0 to f_log_count- 1 do
          display(m_c_log_list[l_log]);
      end; // display_log_list

    procedure c_log_list.write_string(p_log, p_text: String);
      var l_index_of: Integer;
      begin
        l_index_of:= f_index_of(p_log);
        if l_index_of>= 0
          then f_c_log(l_index_of).write_string(p_text)
          else display_bug_stop('c_log_list.no log_journal '+ p_log);
      end; // write_string

    procedure c_log_list.write_line(p_log, p_text: String);
      var l_index_of: Integer;
      begin
        l_index_of:= f_index_of(p_log);
        if l_index_of>= 0
          then f_c_log(l_index_of).write_line(p_text)
          else display_bug_stop('c_log_list_2.no log_journal '+ p_log);
      end; // write_line

    Destructor c_log_list.Destroy;
      var l_log_index: Integer;
      begin
        for l_log_index:= 0 to f_log_count- 1 do
          f_c_log(l_log_index).Free;
        m_c_log_list.Free;

        Inherited;
      end; // Destroy

    begin // u_c_log_list
    end. // u_c_log_list
