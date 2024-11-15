with System;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;

package body Window is

   package IC renames Interfaces.C; use IC;
   package ICS renames IC.Strings;

   CS_VREDRAW          : constant := 16#1#;
   CS_HREDRAW          : constant := 16#2#;

   WM_DESTROY          : constant := 16#2#;
   WM_PAINT            : constant := 16#f#;
   COLOR_BACKGROUND    : constant := 1;
   BLACK_BRUSH         : constant := 4;

   WS_OVERLAPPEDWINDOW : constant := 16#cf0000#;
   WS_VISIBLE          : constant := 16#10000000#;

   SW_SHOW             : constant := 5;

   BI_RGB              : constant := 0;
   DIB_RGB_COLORS      : constant := 0;

   subtype PVOID is System.Address;
   subtype PCVOID is PVOID;
   subtype HANDLE is PVOID;
   subtype LPVOID is PVOID;
   subtype HWND is HANDLE;
   subtype HDC is HANDLE;
   subtype HMENU is HANDLE;
   subtype HINSTANCE is HANDLE;
   subtype HICON is HANDLE;
   subtype HCURSOR is HANDLE;
   subtype HBRUSH is HANDLE;
   subtype HGDIOBJ is HANDLE;

   subtype DWORD is IC.unsigned_long;

   type ATOM is new IC.unsigned_short;

   type BITMAPINFOHEADER is record
      biSize          : DWORD   := 0;
      biWidth         : Ic.long := 0;
      biHeight        : Ic.long := 0;
      biPlanes        : Ic.long := 0;
      biBitCount      : Ic.long := 0;
      biCompression   : DWORD   := 0;
      biSizeImage     : DWORD   := 0;
      biXPelsPerMeter : Ic.long := 0;
      biYPelsPerMeter : Ic.long := 0;
      biClrUsed       : DWORD   := 0;
      biClrImportant  : DWORD   := 0;
   end record;

   type Byte is mod 2**8 with size => 8; 
   type Byte_Array is array (Natural range <>) of Byte;

   type RGBQUAD is record
      rgbBlue     : Byte := 0;
      rgbGreen    : Byte := 0;
      rgbRed      : Byte := 0;
      rgbReserved : Byte := 0;
   end record;

   ANYSIZE_ARRAY : constant := 0;
   type RGBQUAD_Array is array (Integer range 0 .. ANYSIZE_ARRAY) of aliased RGBQUAD;

   type BITMAPINFO is record
      bmiHeader : BITMAPINFOHEADER;
      bmiColors : RGBQUAD_Array;
   end record;

   function Check (A : ATOM) return Boolean is
      (if A = 0 then False else True);

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

   function TO_LPCSTR is new Ada.Unchecked_Conversion (IC.Strings.chars_ptr, LPCSTR);

   Lp_Window_Name : LPCSTR := TO_LPCSTR (ICS.New_String ("Ada Window"));
   Lp_Class_Name  : LPCSTR := TO_LPCSTR (ICS.New_String ("Core"));
   Lp_Menu_Name   : LPCSTR := TO_LPCSTR (ICS.New_String (""));

   type WNDCLASS is record
      Style           : IC.unsigned := CS_HREDRAW or CS_VREDRAW;
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

   type RECT is record
      Left   : IC.long := 0;
      Top    : IC.long := 0;
      Right  : IC.long := 0;
      Bottom : IC.long := 0;
   end record;
   type RECT_Access is access all RECT;

   type PAINTSTRUCT is record
      H_dc         : HDC :=  System.Null_Address;
      F_Erase      : Boolean := False;
      Rc_Paint     : RECT;
      F_Restore    : Boolean := False;
      F_Inc_Update : Boolean := False;
      Rgb_Reserved : Byte_Array (0 .. 31) := (others => 0);
   end record;

   function Begin_Paint (H_Wnd : HWND; Lp_Paint : access PAINTSTRUCT) return HDC
   with Import => True, Convention => C, External_Name => "BeginPaint";

   function End_Paint (H_Wnd : HWND; Lp_Paint : access PAINTSTRUCT) return Boolean
   with Import => True, Convention => C, External_Name => "EndPaint";

   function Set_DI_Bits_To_Device
     (H_dc         : HDC;
      X_Dest       : IC.int;
      Y_Dest       : IC.int;
      Dw_Width     : DWORD;
      Dw_Height    : DWORD;
      X_Src        : IC.int;
      Y_Src        : IC.int;
      U_Start_Scan : IC.int;
      C_Scan_Lines : IC.unsigned;
      Lpv_Bits     : PCVOID;
      Lp_Bmi       : access BITMAPINFO;
      Fu_Color_Use : IC.unsigned) return IC.int
   with Import => True, Convention => C, External_Name => "SetDIBitsToDevice";

   procedure Post_Quit_Message (N_Exit_Code : IC.int)
   with Import => True, Convention => C, External_Name => "PostQuitMessage";

   function Def_Window_Proc
     (H_Wnd   : HWND;
      Msg     : IC.unsigned;
      W_Param : WPARAM;
      L_Param : LPARAM)
      return LRESULT
    with Import => True, Convention => C, External_Name => "DefWindowProcA";

   function Get_Last_Error return DWORD
   with Import => True, Convention => C, External_Name => "GetLastError";

   function Fill_Rect (H_Dc : HDC; Lp_Rc : System.Address; H_br : HBRUSH) return IC.int
   with Import => True, Convention => C, External_Name => "FillRect";

   function Get_Stock_Object (Fn_Object : IC.int) return HGDIOBJ
   with Import => True, Convention => C, External_Name => "GetStockObject";

   procedure Draw_Buffer (H_Wnd : HWND) is
      PS   : aliased PAINTSTRUCT;
      H_Dc : HDC;
      Bmi  : aliased BITMAPINFO;
      Buffer : Byte_Array := (0, 0, 255, 255,    -- First pixel (BGRA)
                             0, 0, 255, 255,      -- Second pixel
                             0, 0, 255, 255,      -- Third pixel
                             0, 0, 255, 255);     -- Fourth pixel
      R_Set_DI : IC.int;
      R_End_Paint : Boolean;
      Last_Err : DWORD;
   begin
      Put_Line ("Window Handle: " & H_Wnd'Image);
      H_Dc := Begin_Paint (H_Wnd, PS'Access);
   
      Put_Line (H_Dc'Image);
      Bmi.bmiHeader.biSize := BITMAPINFOHEADER'Size;
      bmi.bmiHeader.biWidth := 2;
      bmi.bmiHeader.biHeight := -2;
      bmi.bmiHeader.biPlanes := 1;
      bmi.bmiHeader.biBitCount := 32;
      bmi.bmiHeader.biCompression := BI_RGB;
      
      R_Set_DI := Set_DI_Bits_To_Device (
         H_Dc, 0, 0,           -- Destination x, y
         2, 2,                 -- Width, Height
         0, 0,                 -- Source x, y
         0, 2,                 -- First scan line, number of scan lines
         Buffer'Address,       -- Array of bits
         Bmi'Access,          -- Bitmap info
         DIB_RGB_COLORS       -- RGB or Palette
      );
      
      if R_Set_DI = 0 then
         Last_Err := Get_Last_Error;
         Put_Line ("SetDIBitsToDevice failed with error: " & Last_Err'Image);
      end if;
   
      R_End_Paint := End_Paint (H_Wnd, PS'Access);
      
      Last_Err := Get_Last_Error;
      Put_Line (Last_Err'Image);
   end Draw_Buffer;

   procedure Fill_Black (H_Wnd : HWND) is
      PS : aliased PAINTSTRUCT;
      H_Dc : HDC := Begin_Paint (H_Wnd, PS'Access);
      Res_Fill : IC.int;
      Res_Bool : Boolean;
   begin
      Res_Fill := Fill_Rect (H_Dc, PS.Rc_Paint'Address, HBRUSH (Get_Stock_Object (BLACK_BRUSH)));
      Res_Bool := End_Paint (H_Wnd, PS'Access);
   end;

   function Wnd_Proc (H_Wnd   : HWND; 
                      Msg     : IC.unsigned; 
                      W_Param : WPARAM; 
                      L_Param : LPARAM) return LRESULT is
   begin
        case Msg is
            when WM_DESTROY =>
               Post_Quit_Message (0);
            when WM_PAINT =>
               Fill_Black (H_Wnd);
               --  Draw_Buffer (H_Wnd);
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

   function Show_Window (H_Wnd : HWND; N_Cmd_Show : IC.int) return Boolean
   with Import => True, Convention => C, External_Name => "ShowWindow"; 

   function Update_Window (H_Wnd : HWND) return Boolean
   with Import => True, Convention => C, External_Name => "UpdateWindow"; 

   function Get_Message (Lp_Msg : MSG_Access; H_Wnd : HWND;
                         W_Msg_Filter_Min : IC.unsigned;
                         W_Msg_Filter_Max : IC.unsigned) return Boolean
   with Import => True, Convention => C, External_Name => "GetMessageA";

   function Dispatch_Message (Lp_Msg : MSG_Access) return LRESULT
   with Import => True, Convention => C, External_Name => "DispatchMessageA";

   procedure Window is
    WC             : aliased WNDCLASS;
    H_Instance     : HINSTANCE := Get_H_Instance;
    Res_Atom       : ATOM;
    H_Wnd          : HWND := System.Null_Address;
    Res_Bool       : Boolean;
    use IC;

   begin
        WC.Lp_fn_Wnd_Proc  := Wnd_Proc'Access;
        WC.H_Instance      := H_Instance;
        WC.H_br_Background := HBRUSH (Get_Stock_Object (COLOR_BACKGROUND));
        Res_Atom := Register_Class (WC'Access);
        if Check (Res_Atom) then
            H_Wnd := Create_Window (0,
                                  Lp_Class_Name,
                                  Lp_Window_Name,
                                  WS_OVERLAPPEDWINDOW or WS_VISIBLE,
                                  0, 0, 640, 480,
                                  System.Null_Address,
                                  System.Null_Address,
                                  H_Instance,
                                  System.Null_Address);
        end if;
        Put_Line (H_Wnd'Image);
        --Res_Bool := Show_Window (H_Wnd, SW_SHOW);
        --Put_Line (Res_Bool'Image);
        Res_Bool := Update_Window (H_Wnd);
        Put_Line (Res_Bool'Image);
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
   end Window;
end Window;
