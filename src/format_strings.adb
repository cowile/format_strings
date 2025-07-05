with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Format_Strings is

   type Parse_State is (Normal, Escape, In_Hole);

   --  Simple string buffer implementation
   type String_Buffer is record
      Data : Unbounded_String;
   end record;

   procedure Append (Buf : in out String_Buffer; S : String) is
   begin
      Append (Buf.Data, S);
   end Append;

   procedure Append (Buf : in out String_Buffer; C : Character) is
   begin
      Append (Buf.Data, C);
   end Append;

   function To_String (Buf : String_Buffer) return String is
   begin
      return To_String (Buf.Data);
   end To_String;

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
            if Spec.Alt_Form and Value /= 0 then
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
            -- decimal
            Append (Img, Trim (Integer'Image (abs Value), Ada.Strings.Both));
      end case;

      declare
         Result : constant String := To_String (Img);
      begin
         --  Apply width formatting
         if Spec.Width > Result'Length then
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

   --  Helper to process template and format one argument
   procedure Process_Template_1
     (Template : String; Arg : String; Result : out Unbounded_String)
   is
      State      : Parse_State := Normal;
      Hole_Start : Natural := 0;
      Buf        : String_Buffer;
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
                  Append (Buf, Template (I));
               end if;

            when Escape =>
               Append (Buf, Template (I));
               State := Normal;

            when In_Hole =>
               if Template (I) = '}' then
                  --  Parse the hole content
                  declare
                     Hole_Content : constant String :=
                       Template (Hole_Start + 1 .. I - 1);
                     Colon_Pos    : Natural := 0;
                  begin
                     --  Find colon separator
                     for J in Hole_Content'Range loop
                        if Hole_Content (J) = ':' then
                           Colon_Pos := J;
                           exit;
                        end if;
                     end loop;

                     if Colon_Pos > 0 then
                        --  Parse format spec
                        declare
                           Spec : constant Format_Spec :=
                             Parse_Spec
                               (Hole_Content
                                  (Colon_Pos + 1 .. Hole_Content'Last));
                        begin
                           Append
                             (Buf, Arg);  -- Simplified - ignores spec for now
                        end;
                     else
                        Append (Buf, Arg);
                     end if;
                  end;
                  State := Normal;
               elsif Template (I) = '\' then
                  State := Escape;
               end if;
         end case;
      end loop;

      Result := Buf.Data;
   end Process_Template_1;

   function Format (Template : String; Arg : Integer) return String is
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
                              Append (Result, Format_Integer (Arg, Spec));
                           end;
                        else
                           Append
                             (Result, Format_Integer (Arg, (others => <>)));
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

   function Format (Template : String; Arg : String) return String is
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
   end Format;

   function Format (Template : String; Arg1, Arg2 : Integer) return String is
      Result          : Unbounded_String;
      State           : Parse_State := Normal;
      Hole_Start      : Natural := 0;
      Hole_Count      : Natural := 0;
      Next_Sequential : Positive := 1;
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
                  Hole_Count := Hole_Count + 1;

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

                     --  Format the appropriate argument
                     declare
                        Spec : constant Format_Spec :=
                          (if Colon_Pos > 0
                           then
                             Parse_Spec
                               (Hole_Content
                                  (Colon_Pos + 1 .. Hole_Content'Last))
                           else (others => <>));
                     begin
                        case Position is
                           when 1 =>
                              Append (Result, Format_Integer (Arg1, Spec));

                           when 2 =>
                              Append (Result, Format_Integer (Arg2, Spec));

                           when others =>
                              raise Format_Error
                                with "Invalid position: " & Position'Image;
                        end case;
                     end;
                  end;
                  State := Normal;
               elsif Template (I) = '\' then
                  State := Escape;
               end if;
         end case;
      end loop;

      return To_String (Result);
   end Format;

end Format_Strings;
