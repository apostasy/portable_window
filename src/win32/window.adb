with System;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;

package body Window is

   package IC renames Interfaces.C; use IC;
   package ICS renames IC.Strings;

   WM_DESTROY          : constant := 16#2#;
   COLOR_BACKGROUND    : constant := 1;

   WS_OVERLAPPEDWINDOW : constant := 16#cf0000#;
   WS_VISIBLE          : constant := 16#10000000#;

   SW_SHOW             : constant := 5;

   subtype PVOID is System.Address;
   subtype HANDLE is PVOID;
   subtype LPVOID is PVOID;
   subtype HWND is HANDLE;
   subtype HMENU is HANDLE;
   subtype HINSTANCE is HANDLE;
   subtype HICON is HANDLE;
   subtype HCURSOR is HANDLE;
   subtype HBRUSH is HANDLE;
   subtype HGDIOBJ is HANDLE;

   subtype DWORD is IC.unsigned_long;

   type ATOM is new IC.unsigned_short;

   function Check (A : ATOM) return Boolean is
      (if A = 0 then False else True);

   --type LPCSTR is access all String;
   type LPCSTR is access constant IC.char;
   type LPSTR is access all IC.char;

   IDI_APPLICATION : LPCSTR;
   IDC_ARROW       : LPCSTR;

   type WPARAM is mod 2 ** Standard'Address_Size;

   type LONG_PTR is
     range -(2 ** (Standard'Address_Size - 1))
           .. +(2 ** (Standard'Address_Size - 1) - 1);
   subtype LPARAM is LONG_PTR;
   subtype LRESULT is LONG_PTR;

   type WNDPROC is
     access function
       (H_Wnd : HWND; Msg : IC.unsigned; W_Param : WPARAM; L_Param : LPARAM)
        return LRESULT;

   function Load_Icon
     (H_Instance : HINSTANCE; Lp_Icon_Name : LPCSTR) return HICON
   with Import => True, Convention => C, External_Name => "LoadIconA";

   function Load_Cursor
     (H_Instance : HINSTANCE; Lp_Cursor_Name : LPCSTR) return HCURSOR
   with Import => True, Convention => C, External_Name => "LoadCursorA";

   function CHARPTR_TO_LPCSTR is new Ada.Unchecked_Conversion (IC.Strings.chars_ptr, LPCSTR);

   title : IC.Strings.chars_ptr := ICS.New_String ("Ada Window");
   Lp_Window_Name     : LPCSTR := CHARPTR_TO_LPCSTR (title);
   class : IC.Strings.chars_ptr := ICS.New_String ("Core");
   Lp_Class_Name     : LPCSTR := CHARPTR_TO_LPCSTR (class);
   menu : IC.Strings.chars_ptr := ICS.New_String ("");
   Lp_Menu_Name     : LPCSTR := CHARPTR_TO_LPCSTR (menu);

   type WNDCLASS is record
      Style           : IC.unsigned := 0;
      Lp_fn_Wnd_Proc  : WNDPROC;
      Cb_Cls_Extra    : IC.int := 0;
      Cb_Wnd_Extra    : IC.int := 0;
      H_Instance      : HINSTANCE := System.Null_Address;
      H_Icon          : HICON   := Load_Icon (System.Null_Address, IDI_APPLICATION);
      H_Cursor        : HCURSOR := Load_Cursor (System.Null_Address, IDC_ARROW);
      H_br_Background : HBRUSH  := System.Null_Address;
      Lpsz_Menu_Name  : LPCSTR  := Lp_Menu_Name;
      Lpsz_Class_Name : LPCSTR  := Lp_Class_Name;
   end record;
   type WNDCLASS_Access is access all WNDCLASS;

   type POINT is record
      X, Y : IC.long;
   end record;

   type MSG is record
      H_Wnd   : HWND;
      Message : IC.unsigned;
      W_Param : WPARAM;
      L_Param : LPARAM;
      Time    : DWORD;
      Pt      : POINT;
   end record;
   type MSG_Access is access all MSG;

   procedure Post_Quit_Message (N_Exit_Code : IC.int)
   with Import => True, Convention => C, External_Name => "PostQuitMessage";

   function Def_Window_Proc
     (H_Wnd   : HWND;
      Msg     : IC.unsigned;
      W_Param : WPARAM;
      L_Param : LPARAM)
      return LRESULT
    with Import => True, Convention => C, External_Name => "DefWindowProcA";

   function Wnd_Proc (H_Wnd   : HWND; 
                      Msg     : IC.unsigned; 
                      W_Param : WPARAM; 
                      L_Param : LPARAM) return LRESULT is
   begin
        case Msg is
            when WM_DESTROY =>
                Post_Quit_Message (0);
            when others =>
                return Def_Window_Proc(H_Wnd, Msg, W_Param, L_Param);
        end case;
        return 0;
   end Wnd_Proc;

   function Get_H_Instance return HINSTANCE is
      function Retrieve_H_Instance return HINSTANCE;
      pragma Import (C, Retrieve_H_Instance, "rts_get_hInstance");
   begin
      return Retrieve_H_Instance;
   end;

   function Get_Stock_Object (Fn_Object : IC.int) return HGDIOBJ
   with Import => True, Convention => C, External_Name => "GetStockObject";

   function Register_Class (Lp_Wnd_Class : access WNDCLASS) return ATOM
   with Import => True, Convention => C, External_Name => "RegisterClassA";

   function Create_Window
     (Dw_Ex_Style    : DWORD;
      Lp_Class_Name  : LPCSTR;
      Lp_Window_Name : LPCSTR;
      Dw_Style       : DWORD;
      X              : IC.int;
      Y              : IC.int;
      Width          : IC.int;
      Height         : IC.int;
      H_Wnd_Parent   : HWND;
      H_Menu         : HMENU;
      H_Instance     : HINSTANCE;
      Lp_Param       : LPVOID) return HWND
   with Import => True, Convention => C, External_Name => "CreateWindowExA";

   --  function Show_Window (H_Wnd : HWND; N_Cmd_Show : IC.int) return Boolean
   --  with Import => True, Convention => C, External_Name => "ShowWindow";

   function Get_Message (Lp_Msg : MSG_Access; H_Wnd : HWND;
                         W_Msg_Filter_Min : IC.unsigned;
                         W_Msg_Filter_Max : IC.unsigned) return Boolean
   with Import => True, Convention => C, External_Name => "GetMessageA";

   function Dispatch_Message (Lp_Msg : MSG_Access) return LRESULT
   with Import => True, Convention => C, External_Name => "DispatchMessageA";

   procedure Window is
    WC             : aliased WNDCLASS;
    --Lp_Class_Name  : LPCSTR := new String' ("Core" & ASCII.Nul);
    --Lp_Window_Name : LPCSTR := new String' ("Ada Window" & ASCII.Nul);
    H_Instance     : HINSTANCE := Get_H_Instance;
    Res            : ATOM;
    Win            : HWND := System.Null_Address;
    use IC;
   begin
        WC.Lp_fn_Wnd_Proc  := Wnd_Proc'Access;
        WC.H_Instance      := H_Instance;
        WC.H_br_Background := HBRUSH (Get_Stock_Object (COLOR_BACKGROUND));
        Res := Register_Class (WC'Access);
        if Check (Res) then
            Win := Create_Window (0,
                                  Lp_Class_Name,
                                  Lp_Window_Name,
                                  WS_OVERLAPPEDWINDOW or WS_VISIBLE,
                                  0, 0, 640, 480,
                                  System.Null_Address,
                                  System.Null_Address,
                                  H_Instance,
                                  System.Null_Address);
        end if;
        declare
            Message        : MSG_Access := new MSG;
            Has_Msg        : Boolean := True;
            Lp_Result      : LRESULT;
        begin
            while Has_Msg loop
               Lp_Result := Dispatch_Message (Message);
               Has_Msg := Get_Message (Message, System.Null_Address, 0, 0);
            end loop;
        end;
       
   end;
end Window;
