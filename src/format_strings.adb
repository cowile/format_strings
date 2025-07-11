with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Format_Strings is

   type Parse_State is (Normal, Escape, In_Hole);

   --  Common template parsing logic extracted to eliminate duplication
   type Hole_Info is record
      Position : Natural;
      Spec     : Format_Spec;
   end record;

   type Hole_Array is array (Positive range <>) of Hole_Info;

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
                     --  Parse hole content
                     declare
                        Hole_Content : constant String :=
                          Template (Hole_Start + 1 .. I - 1);
                        Colon_Pos    : Natural := 0;
                        Position     : Natural := 0;
                     begin
                        --  Find colon separator
                        for J in Hole_Content'Range loop
                           if Hole_Content (J) = ':' then
                              Colon_Pos := J;
                              exit;
                           end if;
                        end loop;

                        --  Determine position
                        if Colon_Pos > 0 and then Hole_Content'Length > 0 then
                           declare
                              Pos_Str : constant String :=
                                Hole_Content
                                  (Hole_Content'First .. Colon_Pos - 1);
                           begin
                              if Pos_Str'Length > 0
                                and then Pos_Str (Pos_Str'First) in '0' .. '9'
                              then
                                 Position := Natural'Value (Pos_Str);
                              end if;
                           end;
                        elsif Hole_Content'Length > 0
                          and then Hole_Content (Hole_Content'First) in
                                     '0' .. '9'
                        then
                           Position := Natural'Value (Hole_Content);
                        end if;

                        --  Use sequential if no position specified
                        if Position = 0 then
                           Position := Next_Sequential;
                           Next_Sequential := Next_Sequential + 1;
                        end if;

                        --  Parse format spec
                        declare
                           Spec : constant Format_Spec :=
                             (if Colon_Pos > 0
                              then
                                Parse_Spec
                                  (Hole_Content
                                     (Colon_Pos + 1 .. Hole_Content'Last))
                              else (others => <>));
                        begin
                           Holes (Current_Hole) :=
                             (Position => Position, Spec => Spec);
                           Hole_Count := Current_Hole;
                        end;
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
                  --  Parse hole content
                  declare
                     Hole_Content : constant String :=
                       Template (Hole_Start + 1 .. I - 1);
                     Colon_Pos    : Natural := 0;
                     Position     : Natural := 0;
                  begin
                     --  Find colon separator
                     for J in Hole_Content'Range loop
                        if Hole_Content (J) = ':' then
                           Colon_Pos := J;
                           exit;
                        end if;
                     end loop;

                     --  Determine position
                     if Colon_Pos > 0 and then Hole_Content'Length > 0 then
                        declare
                           Pos_Str : constant String :=
                             Hole_Content
                               (Hole_Content'First .. Colon_Pos - 1);
                        begin
                           if Pos_Str'Length > 0
                             and then Pos_Str (Pos_Str'First) in '0' .. '9'
                           then
                              Position := Natural'Value (Pos_Str);
                           end if;
                        end;
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

                     --  Parse format spec
                     declare
                        Spec : constant Format_Spec :=
                          (if Colon_Pos > 0
                           then
                             Parse_Spec
                               (Hole_Content
                                  (Colon_Pos + 1 .. Hole_Content'Last))
                           else (others => <>));
                     begin
                        Process_Hole (Position, Spec, Result);
                     end;
                  end;
                  State := Normal;
               elsif Template (I) = '\' then
                  State := Escape;
               end if;
         end case;
      end loop;
   end Process_Template;

   --  Format integers according to spec
   function Format_Integer (Value : Integer; Spec : Format_Spec) return String
   is
      use Ada.Strings.Fixed;

      function To_Base (V : Natural; Base : Positive) return String is
         Hex_Digits : constant String := "0123456789abcdef";
         Result     : String (1 .. 64);
         Pos        : Natural := Result'Last;
         Val        : Natural := V;
      begin
         if Val = 0 then
            return "0";
         end if;

         while Val > 0 loop
            Result (Pos) := Hex_Digits ((Val mod Base) + 1);
            Val := Val / Base;
            Pos := Pos - 1;
         end loop;

         return Result (Pos + 1 .. Result'Last);
      end To_Base;

      Img : Unbounded_String;
   begin
      --  Handle sign
      if Value < 0 then
         Append (Img, '-');
      elsif Spec.Show_Sign then
         Append (Img, '+');
      end if;

      --  Convert to appropriate base
      case Spec.Type_Char is
         when 'b' =>
            if Spec.Alt_Form then
               Append (Img, "0b");
            end if;
            Append (Img, To_Base (abs Value, 2));

         when 'o' =>
            if Spec.Alt_Form and then Value /= 0 then
               Append (Img, "0");
            end if;
            Append (Img, To_Base (abs Value, 8));

         when 'x' =>
            if Spec.Alt_Form then
               Append (Img, "0x");
            end if;
            Append (Img, To_Base (abs Value, 16));

         when 'X' =>
            if Spec.Alt_Form then
               Append (Img, "0X");
            end if;
            declare
               Hex : constant String := To_Base (abs Value, 16);
            begin
               for C of Hex loop
                  if C in 'a' .. 'f' then
                     Append (Img, Character'Val (Character'Pos (C) - 32));
                  else
                     Append (Img, C);
                  end if;
               end loop;
            end;

         when others =>
            --  decimal
            Append (Img, Trim (Integer'Image (abs Value), Ada.Strings.Both));
      end case;

      declare
         Result : constant String := To_String (Img);
      begin
         --  Apply width formatting with zero-padding support
         if Spec.Width > Result'Length then
            --  Zero padding needs special handling to put zeros after sign
            if Spec.Zero_Pad and then Spec.Alignment = Default then
               declare
                  Sign_Len        : Natural := 0;
                  Base_Prefix_Len : Natural := 0;
                  Number_Start    : Positive := Result'First;
               begin
                  --  Check for sign
                  if Result'Length > 0
                    and then Result (Result'First) in '+' | '-'
                  then
                     Sign_Len := 1;
                     Number_Start := Number_Start + 1;
                  end if;

                  --  Check for base prefix (0x, 0b, etc)
                  if Number_Start <= Result'Last - 1
                    and then Result (Number_Start) = '0'
                    and then Result (Number_Start + 1) in 'x' | 'X' | 'b' | 'B'
                  then
                     Base_Prefix_Len := 2;
                     Number_Start := Number_Start + 2;
                  elsif Number_Start <= Result'Last
                    and then Result (Number_Start) = '0'
                    and then Spec.Type_Char = 'o'
                    and then Spec.Alt_Form
                  then
                     Base_Prefix_Len := 1;
                     Number_Start := Number_Start + 1;
                  end if;

                  --  Build result with zeros in the right place
                  return
                    Result
                      (Result'First
                       .. Result'First + Sign_Len + Base_Prefix_Len - 1)
                    & (1 .. Spec.Width - Result'Length => '0')
                    & Result (Number_Start .. Result'Last);
               end;
            end if;

            case Spec.Alignment is
               when Left =>
                  return
                    Result
                    & (1 .. Spec.Width - Result'Length => Spec.Fill_Char);

               when Right | Default =>
                  return
                    (1 .. Spec.Width - Result'Length => Spec.Fill_Char)
                    & Result;

               when Center =>
                  declare
                     Left_Pad  : constant Natural :=
                       (Spec.Width - Result'Length) / 2;
                     Right_Pad : constant Natural :=
                       Spec.Width - Result'Length - Left_Pad;
                  begin
                     return
                       (1 .. Left_Pad => Spec.Fill_Char)
                       & Result
                       & (1 .. Right_Pad => Spec.Fill_Char);
                  end;
            end case;
         else
            return Result;
         end if;
      end;
   end Format_Integer;

   --  Format floats according to spec
   function Format_Float (Value : Float; Spec : Format_Spec) return String is
      use Ada.Strings.Fixed;

      --  Helper to format with specific precision
      function Fixed_Point_Image (V : Float; Precision : Natural) return String
      is
         Abs_Val   : constant Float := abs V;
         Int_Part  : constant Integer := Integer (Float'Floor (Abs_Val));
         Frac_Part : constant Float := Abs_Val - Float (Int_Part);

         --  Build fractional part with desired precision
         Frac_String : Unbounded_String;
         Scaled      : Float := Frac_Part;
      begin
         if Precision = 0 then
            return Trim (Integer'Image (Int_Part), Ada.Strings.Left);
         end if;

         Append (Frac_String, '.');
         for I in 1 .. Precision loop
            Scaled := Scaled * 10.0;
            declare
               Digit : constant Integer := Integer (Float'Floor (Scaled));
            begin
               Append
                 (Frac_String, Trim (Integer'Image (Digit), Ada.Strings.Left));
               Scaled := Scaled - Float (Digit);
            end;
         end loop;

         return
           Trim (Integer'Image (Int_Part), Ada.Strings.Left)
           & To_String (Frac_String);
      end Fixed_Point_Image;

      Precision : constant Natural :=
        (if Spec.Has_Precision then Spec.Precision else 6);
      Result    : Unbounded_String;
   begin
      --  Handle sign
      if Value < 0.0 then
         Append (Result, '-');
      elsif Spec.Show_Sign then
         Append (Result, '+');
      end if;

      --  Format based on type specifier
      case Spec.Type_Char is
         when 'e' | 'E' =>
            --  Scientific notation
            declare
               Exp      : Integer := 0;
               Mantissa : Float := abs Value;
            begin
               --  Normalize to 1.0 <= mantissa < 10.0
               if Mantissa /= 0.0 then
                  while Mantissa >= 10.0 loop
                     Mantissa := Mantissa / 10.0;
                     Exp := Exp + 1;
                  end loop;
                  while Mantissa < 1.0 loop
                     Mantissa := Mantissa * 10.0;
                     Exp := Exp - 1;
                  end loop;
               end if;

               Append (Result, Fixed_Point_Image (Mantissa, Precision));
               Append (Result, (if Spec.Type_Char = 'E' then 'E' else 'e'));
               Append (Result, (if Exp >= 0 then '+' else '-'));
               Append
                 (Result, Trim (Integer'Image (abs Exp), Ada.Strings.Left));
            end;

         when 'g' | 'G' =>
            --  General format (shortest of f or e)
            --  Simplified: just use fixed for now
            Append (Result, Fixed_Point_Image (abs Value, Precision));

         when '%' =>
            --  Percentage
            Append (Result, Fixed_Point_Image (abs Value * 100.0, Precision));
            Append (Result, '%');

         when others =>
            --  Fixed point (default)
            Append (Result, Fixed_Point_Image (abs Value, Precision));
      end case;

      declare
         Formatted : constant String := To_String (Result);
      begin
         --  Apply width formatting
         if Spec.Width > Formatted'Length then
            case Spec.Alignment is
               when Left =>
                  return
                    Formatted
                    & (1 .. Spec.Width - Formatted'Length => Spec.Fill_Char);

               when Right | Default =>
                  return
                    (1 .. Spec.Width - Formatted'Length => Spec.Fill_Char)
                    & Formatted;

               when Center =>
                  declare
                     Left_Pad  : constant Natural :=
                       (Spec.Width - Formatted'Length) / 2;
                     Right_Pad : constant Natural :=
                       Spec.Width - Formatted'Length - Left_Pad;
                  begin
                     return
                       (1 .. Left_Pad => Spec.Fill_Char)
                       & Formatted
                       & (1 .. Right_Pad => Spec.Fill_Char);
                  end;
            end case;
         else
            return Formatted;
         end if;
      end;
   end Format_Float;

   --  Format strings according to spec
   function Format_String (Value : String; Spec : Format_Spec) return String is
      Result : constant String :=
        (if Spec.Has_Precision and then Spec.Precision < Value'Length
         then Value (Value'First .. Value'First + Spec.Precision - 1)
         else Value);
   begin

      --  Apply width formatting
      if Spec.Width > Result'Length then
         case Spec.Alignment is
            when Left | Default =>
               return
                 Result & (1 .. Spec.Width - Result'Length => Spec.Fill_Char);

            when Right =>
               return
                 (1 .. Spec.Width - Result'Length => Spec.Fill_Char) & Result;

            when Center =>
               declare
                  Left_Pad  : constant Natural :=
                    (Spec.Width - Result'Length) / 2;
                  Right_Pad : constant Natural :=
                    Spec.Width - Result'Length - Left_Pad;
               begin
                  return
                    (1 .. Left_Pad => Spec.Fill_Char)
                    & Result
                    & (1 .. Right_Pad => Spec.Fill_Char);
               end;
         end case;
      else
         return Result;
      end if;
   end Format_String;

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
      State      : Parse_State := Normal;
      Hole_Start : Natural := 0;
      Hole_Found : Boolean := False;
   begin
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
                  if not Hole_Found then
                     --  Parse and format the argument
                     declare
                        Hole_Content : constant String :=
                          Template (Hole_Start + 1 .. I - 1);
                        Colon_Pos    : Natural := 0;
                     begin
                        for J in Hole_Content'Range loop
                           if Hole_Content (J) = ':' then
                              Colon_Pos := J;
                              exit;
                           end if;
                        end loop;

                        if Colon_Pos > 0 then
                           declare
                              Spec : constant Format_Spec :=
                                Parse_Spec
                                  (Hole_Content
                                     (Colon_Pos + 1 .. Hole_Content'Last));
                           begin
                              Append (Result, Formatter (Arg, Spec));
                           end;
                        else
                           Append (Result, Formatter (Arg, (others => <>)));
                        end if;
                     end;
                     Hole_Found := True;
                  end if;
                  State := Normal;
               elsif Template (I) = '\' then
                  State := Escape;
               end if;
         end case;
      end loop;

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

   --  Backward compatibility wrappers
   function Format_Int (Template : String; Arg : Integer) return String is
      function Fmt is new Format (Integer, Format_Integer);
   begin
      return Fmt (Template, Arg);
   end Format_Int;

   function Format_Float (Template : String; Arg : Float) return String is
      function Fmt is new Format (Float, Format_Float);
   begin
      return Fmt (Template, Arg);
   end Format_Float;

   function Format_Str (Template : String; Arg : String) return String is
      Result     : Unbounded_String;
      State      : Parse_State := Normal;
      Hole_Start : Natural := 0;
      Hole_Found : Boolean := False;
   begin
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
                  if not Hole_Found then
                     --  Parse and format the argument
                     declare
                        Hole_Content : constant String :=
                          Template (Hole_Start + 1 .. I - 1);
                        Colon_Pos    : Natural := 0;
                     begin
                        for J in Hole_Content'Range loop
                           if Hole_Content (J) = ':' then
                              Colon_Pos := J;
                              exit;
                           end if;
                        end loop;

                        if Colon_Pos > 0 then
                           declare
                              Spec : constant Format_Spec :=
                                Parse_Spec
                                  (Hole_Content
                                     (Colon_Pos + 1 .. Hole_Content'Last));
                           begin
                              Append (Result, Format_String (Arg, Spec));
                           end;
                        else
                           Append
                             (Result, Format_String (Arg, (others => <>)));
                        end if;
                     end;
                     Hole_Found := True;
                  end if;
                  State := Normal;
               elsif Template (I) = '\' then
                  State := Escape;
               end if;
         end case;
      end loop;

      return To_String (Result);
   end Format_Str;

   function Format_2_Int
     (Template : String; Arg1, Arg2 : Integer) return String
   is
      Result : Unbounded_String;

      procedure Process_Hole
        (Position : Natural;
         Spec     : Format_Spec;
         Result   : in out Unbounded_String) is
      begin
         case Position is
            when 1 =>
               Append (Result, Format_Integer (Arg1, Spec));

            when 2 =>
               Append (Result, Format_Integer (Arg2, Spec));

            when others =>
               raise Format_Error with "Invalid position: " & Position'Image;
         end case;
      end Process_Hole;

      procedure Process_Template_2_Int is new Process_Template (Process_Hole);
   begin
      Process_Template_2_Int (Template, Result);
      return To_String (Result);
   end Format_2_Int;

end Format_Strings;
