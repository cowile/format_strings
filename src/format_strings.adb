with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Format_Strings is

   type Parse_State is (Normal, Escape, In_Hole);

   --  Common template parsing logic extracted to eliminate duplication
   type Hole_Info is record
      Position : Natural;
      Spec     : Format_Spec;
   end record;

   type Hole_Array is array (Positive range <>) of Hole_Info;

   --  Helper functions to break down complex parsing logic
   function Find_Colon_Separator (Hole_Content : String) return Natural is
   begin
      for J in Hole_Content'Range loop
         if Hole_Content (J) = ':' then
            return J;
         end if;
      end loop;
      return 0;
   end Find_Colon_Separator;

   function Determine_Position
     (Hole_Content    : String;
      Colon_Pos       : Natural;
      Next_Sequential : in out Positive) return Natural
   is
      Position : Natural := 0;
   begin
      --  Parse explicit position before colon
      if Colon_Pos > 0 and then Hole_Content'Length > 0 then
         declare
            Pos_Str : constant String :=
              Hole_Content (Hole_Content'First .. Colon_Pos - 1);
         begin
            if Pos_Str'Length > 0
              and then Pos_Str (Pos_Str'First) in '0' .. '9'
            then
               Position := Natural'Value (Pos_Str);
            end if;
         end;
         --  Parse position-only hole content
      elsif Hole_Content'Length > 0
        and then Hole_Content (Hole_Content'First) in '0' .. '9'
      then
         Position := Natural'Value (Hole_Content);
      end if;

      --  Use sequential if no position specified
      if Position = 0 then
         Position := Next_Sequential;
         Next_Sequential := Next_Sequential + 1;
      end if;

      return Position;
   end Determine_Position;

   procedure Parse_Hole_Content
     (Hole_Content    : String;
      Next_Sequential : in out Positive;
      Position        : out Natural;
      Spec            : out Format_Spec)
   is
      Colon_Pos : constant Natural := Find_Colon_Separator (Hole_Content);
   begin
      Position :=
        Determine_Position (Hole_Content, Colon_Pos, Next_Sequential);

      --  Parse format spec
      if Colon_Pos > 0 then
         Spec :=
           Parse_Spec (Hole_Content (Colon_Pos + 1 .. Hole_Content'Last));
      else
         Spec := (others => <>);
      end if;
   end Parse_Hole_Content;

   procedure Parse_Template_Holes
     (Template   : String;
      Holes      : out Hole_Array;
      Hole_Count : out Natural;
      Literals   : out Unbounded_String)
   is
      Result          : Unbounded_String;
      State           : Parse_State := Normal;
      Hole_Start      : Natural := 0;
      Current_Hole    : Natural := 0;
      Next_Sequential : Positive := 1;
   begin
      Hole_Count := 0;

      for I in Template'Range loop
         case State is
            when Normal =>
               if Template (I) = '\' then
                  State := Escape;
               elsif Template (I) = '{' then
                  State := In_Hole;
                  Hole_Start := I;
               else
                  Append (Result, Template (I));
               end if;

            when Escape =>
               Append (Result, Template (I));
               State := Normal;

            when In_Hole =>
               if Template (I) = '}' then
                  Current_Hole := Current_Hole + 1;

                  if Current_Hole <= Holes'Length then
                     --  Parse hole content using helper functions
                     declare
                        Hole_Content : constant String :=
                          Template (Hole_Start + 1 .. I - 1);
                        Position     : Natural;
                        Spec         : Format_Spec;
                     begin
                        Parse_Hole_Content
                          (Hole_Content, Next_Sequential, Position, Spec);
                        Holes (Current_Hole) :=
                          (Position => Position, Spec => Spec);
                        Hole_Count := Current_Hole;
                     end;
                  end if;

                  State := Normal;
               elsif Template (I) = '\' then
                  State := Escape;
               end if;
         end case;
      end loop;

      Literals := Result;
   end Parse_Template_Holes;

   --  Shared template processing procedure to reduce duplication
   generic
      with
        procedure Process_Hole
          (Position : Natural;
           Spec     : Format_Spec;
           Result   : in out Unbounded_String);
   procedure Process_Template
     (Template : String; Result : out Unbounded_String);

   procedure Process_Template
     (Template : String; Result : out Unbounded_String)
   is
      State           : Parse_State := Normal;
      Hole_Start      : Natural := 0;
      Next_Sequential : Positive := 1;
   begin
      Result := Null_Unbounded_String;

      for I in Template'Range loop
         case State is
            when Normal =>
               if Template (I) = '\' then
                  State := Escape;
               elsif Template (I) = '{' then
                  State := In_Hole;
                  Hole_Start := I;
               else
                  Append (Result, Template (I));
               end if;

            when Escape =>
               Append (Result, Template (I));
               State := Normal;

            when In_Hole =>
               if Template (I) = '}' then
                  --  Parse hole content using helper functions
                  declare
                     Hole_Content : constant String :=
                       Template (Hole_Start + 1 .. I - 1);
                     Position     : Natural;
                     Spec         : Format_Spec;
                  begin
                     Parse_Hole_Content
                       (Hole_Content, Next_Sequential, Position, Spec);
                     Process_Hole (Position, Spec, Result);
                  end;
                  State := Normal;
               elsif Template (I) = '\' then
                  State := Escape;
               end if;
         end case;
      end loop;
   end Process_Template;

   function Parse_Spec (Spec_String : String) return Format_Spec is
      Result : Format_Spec;
      Index  : Natural := Spec_String'First;

      procedure Skip_Whitespace is
      begin
         while Index <= Spec_String'Last and then Spec_String (Index) = ' '
         loop
            Index := Index + 1;
         end loop;
      end Skip_Whitespace;

      function Parse_Number return Natural is
         Start : constant Natural := Index;
      begin
         while Index <= Spec_String'Last
           and then Spec_String (Index) in '0' .. '9'
         loop
            Index := Index + 1;
         end loop;

         if Index > Start then
            return Natural'Value (Spec_String (Start .. Index - 1));
         else
            return 0;
         end if;
      end Parse_Number;

   begin
      if Spec_String'Length = 0 then
         return Result;
      end if;

      Skip_Whitespace;

      --  Parse fill character and alignment
      if Index <= Spec_String'Last - 1 then
         declare
            Next_Char : constant Character :=
              (if Index < Spec_String'Last then Spec_String (Index + 1)
               else ' ');
         begin
            if Next_Char in '<' | '>' | '^' then
               Result.Fill_Char := Spec_String (Index);
               Index := Index + 1;
            end if;
         end;
      end if;

      --  Parse alignment
      if Index <= Spec_String'Last
        and then Spec_String (Index) in '<' | '>' | '^'
      then
         case Spec_String (Index) is
            when '<' =>
               Result.Alignment := Left;

            when '>' =>
               Result.Alignment := Right;

            when '^' =>
               Result.Alignment := Center;

            when others =>
               null;
         end case;
         Index := Index + 1;
      end if;

      --  Parse sign
      if Index <= Spec_String'Last and then Spec_String (Index) = '+' then
         Result.Show_Sign := True;
         Index := Index + 1;
      end if;

      --  Parse alternate form
      if Index <= Spec_String'Last and then Spec_String (Index) = '#' then
         Result.Alt_Form := True;
         Index := Index + 1;
      end if;

      --  Parse zero padding
      if Index <= Spec_String'Last and then Spec_String (Index) = '0' then
         Result.Zero_Pad := True;
         Index := Index + 1;
      end if;

      --  Parse width
      Result.Width := Parse_Number;

      --  Parse precision
      if Index <= Spec_String'Last and then Spec_String (Index) = '.' then
         Index := Index + 1;
         Result.Has_Precision := True;
         Result.Precision := Parse_Number;
      end if;

      --  Parse type specifier
      if Index <= Spec_String'Last then
         Result.Type_Char := Spec_String (Index);
      end if;

      return Result;
   end Parse_Spec;

   function Count_Holes (Template : String) return Natural is
      State : Parse_State := Normal;
      Count : Natural := 0;
   begin
      for C of Template loop
         case State is
            when Normal =>
               if C = '\' then
                  State := Escape;
               elsif C = '{' then
                  State := In_Hole;
               end if;

            when Escape =>
               State := Normal;

            when In_Hole =>
               if C = '}' then
                  Count := Count + 1;
                  State := Normal;
               elsif C = '\' then
                  State := Escape;
               end if;
         end case;
      end loop;

      return Count;
   end Count_Holes;

   --  Generic single argument formatter
   function Format (Template : String; Arg : T) return String is
      Result     : Unbounded_String;
      Hole_Found : Boolean := False;

      procedure Process_Hole
        (Position : Natural;
         Spec     : Format_Spec;
         Result   : in out Unbounded_String)
      is
         pragma Unreferenced (Position);
      begin
         --  Single argument formatter only processes the first hole
         if not Hole_Found then
            Append (Result, Formatter (Arg, Spec));
            Hole_Found := True;
         end if;
      end Process_Hole;

      procedure Process_Template_Single is new Process_Template (Process_Hole);
   begin
      Process_Template_Single (Template, Result);
      return To_String (Result);
   end Format;

   --  Generic two-argument formatter
   function Format_2 (Template : String; Arg1 : T1; Arg2 : T2) return String is
      Result : Unbounded_String;

      procedure Process_Hole
        (Position : Natural;
         Spec     : Format_Spec;
         Result   : in out Unbounded_String) is
      begin
         case Position is
            when 1 =>
               Append (Result, Formatter_1 (Arg1, Spec));

            when 2 =>
               Append (Result, Formatter_2 (Arg2, Spec));

            when others =>
               raise Format_Error with "Invalid position: " & Position'Image;
         end case;
      end Process_Hole;

      procedure Process_Template_2 is new Process_Template (Process_Hole);
   begin
      Process_Template_2 (Template, Result);
      return To_String (Result);
   end Format_2;

   --  Generic three-argument formatter
   function Format_3
     (Template : String; Arg1 : T1; Arg2 : T2; Arg3 : T3) return String
   is
      Result : Unbounded_String;

      procedure Process_Hole
        (Position : Natural;
         Spec     : Format_Spec;
         Result   : in out Unbounded_String) is
      begin
         case Position is
            when 1 =>
               Append (Result, Formatter_1 (Arg1, Spec));

            when 2 =>
               Append (Result, Formatter_2 (Arg2, Spec));

            when 3 =>
               Append (Result, Formatter_3 (Arg3, Spec));

            when others =>
               raise Format_Error with "Invalid position: " & Position'Image;
         end case;
      end Process_Hole;

      procedure Process_Template_3 is new Process_Template (Process_Hole);
   begin
      Process_Template_3 (Template, Result);
      return To_String (Result);
   end Format_3;

   --  Generic four-argument formatter
   function Format_4
     (Template : String; Arg1 : T1; Arg2 : T2; Arg3 : T3; Arg4 : T4)
      return String
   is
      Result : Unbounded_String;

      procedure Process_Hole
        (Position : Natural;
         Spec     : Format_Spec;
         Result   : in out Unbounded_String) is
      begin
         case Position is
            when 1 =>
               Append (Result, Formatter_1 (Arg1, Spec));

            when 2 =>
               Append (Result, Formatter_2 (Arg2, Spec));

            when 3 =>
               Append (Result, Formatter_3 (Arg3, Spec));

            when 4 =>
               Append (Result, Formatter_4 (Arg4, Spec));

            when others =>
               raise Format_Error with "Invalid position: " & Position'Image;
         end case;
      end Process_Hole;

      procedure Process_Template_4 is new Process_Template (Process_Hole);
   begin
      Process_Template_4 (Template, Result);
      return To_String (Result);
   end Format_4;

   --  Generic five-argument formatter
   function Format_5
     (Template : String; Arg1 : T1; Arg2 : T2; Arg3 : T3; Arg4 : T4; Arg5 : T5)
      return String
   is
      Result : Unbounded_String;

      procedure Process_Hole
        (Position : Natural;
         Spec     : Format_Spec;
         Result   : in out Unbounded_String) is
      begin
         case Position is
            when 1 =>
               Append (Result, Formatter_1 (Arg1, Spec));

            when 2 =>
               Append (Result, Formatter_2 (Arg2, Spec));

            when 3 =>
               Append (Result, Formatter_3 (Arg3, Spec));

            when 4 =>
               Append (Result, Formatter_4 (Arg4, Spec));

            when 5 =>
               Append (Result, Formatter_5 (Arg5, Spec));

            when others =>
               raise Format_Error with "Invalid position: " & Position'Image;
         end case;
      end Process_Hole;

      procedure Process_Template_5 is new Process_Template (Process_Hole);
   begin
      Process_Template_5 (Template, Result);
      return To_String (Result);
   end Format_5;

   --  Generic six-argument formatter
   function Format_6
     (Template : String;
      Arg1     : T1;
      Arg2     : T2;
      Arg3     : T3;
      Arg4     : T4;
      Arg5     : T5;
      Arg6     : T6) return String
   is
      Result : Unbounded_String;

      procedure Process_Hole
        (Position : Natural;
         Spec     : Format_Spec;
         Result   : in out Unbounded_String) is
      begin
         case Position is
            when 1 =>
               Append (Result, Formatter_1 (Arg1, Spec));

            when 2 =>
               Append (Result, Formatter_2 (Arg2, Spec));

            when 3 =>
               Append (Result, Formatter_3 (Arg3, Spec));

            when 4 =>
               Append (Result, Formatter_4 (Arg4, Spec));

            when 5 =>
               Append (Result, Formatter_5 (Arg5, Spec));

            when 6 =>
               Append (Result, Formatter_6 (Arg6, Spec));

            when others =>
               raise Format_Error with "Invalid position: " & Position'Image;
         end case;
      end Process_Hole;

      procedure Process_Template_6 is new Process_Template (Process_Hole);
   begin
      Process_Template_6 (Template, Result);
      return To_String (Result);
   end Format_6;

   --  Generic seven-argument formatter
   function Format_7
     (Template : String;
      Arg1     : T1;
      Arg2     : T2;
      Arg3     : T3;
      Arg4     : T4;
      Arg5     : T5;
      Arg6     : T6;
      Arg7     : T7) return String
   is
      Result : Unbounded_String;

      procedure Process_Hole
        (Position : Natural;
         Spec     : Format_Spec;
         Result   : in out Unbounded_String) is
      begin
         case Position is
            when 1 =>
               Append (Result, Formatter_1 (Arg1, Spec));

            when 2 =>
               Append (Result, Formatter_2 (Arg2, Spec));

            when 3 =>
               Append (Result, Formatter_3 (Arg3, Spec));

            when 4 =>
               Append (Result, Formatter_4 (Arg4, Spec));

            when 5 =>
               Append (Result, Formatter_5 (Arg5, Spec));

            when 6 =>
               Append (Result, Formatter_6 (Arg6, Spec));

            when 7 =>
               Append (Result, Formatter_7 (Arg7, Spec));

            when others =>
               raise Format_Error with "Invalid position: " & Position'Image;
         end case;
      end Process_Hole;

      procedure Process_Template_7 is new Process_Template (Process_Hole);
   begin
      Process_Template_7 (Template, Result);
      return To_String (Result);
   end Format_7;

end Format_Strings;
