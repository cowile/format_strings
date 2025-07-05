with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Format_Strings.Formatters is

   function Default_Image (Item : T; Spec : Format_Spec) return String is
   begin
      --  Default implementation uses 'Image
      return T'Image (Item);
   end Default_Image;

   function Integer_Formatter
     (Item : Int_Type; Spec : Format_Spec) return String
   is
      use Ada.Strings.Fixed;

      --  Convert to Long_Long_Integer to reuse existing Format_Integer
      Value : constant Long_Long_Integer := Long_Long_Integer (Item);

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
            Append (Img, To_Base (Natural (abs Value), 2));

         when 'o' =>
            if Spec.Alt_Form and then Value /= 0 then
               Append (Img, "0");
            end if;
            Append (Img, To_Base (Natural (abs Value), 8));

         when 'x' =>
            if Spec.Alt_Form then
               Append (Img, "0x");
            end if;
            Append (Img, To_Base (Natural (abs Value), 16));

         when 'X' =>
            if Spec.Alt_Form then
               Append (Img, "0X");
            end if;
            declare
               Hex : constant String := To_Base (Natural (abs Value), 16);
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
            --  Decimal
            Append
              (Img,
               Trim (Long_Long_Integer'Image (abs Value), Ada.Strings.Both));
      end case;

      declare
         Result : constant String := To_String (Img);
      begin
         --  Apply width formatting with zero-padding support
         if Spec.Width > Result'Length then
            --  Zero padding needs special handling
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

                  --  Check for base prefix
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
   end Integer_Formatter;

   function Modular_Formatter
     (Item : Mod_Type; Spec : Format_Spec) return String
   is
      --  Convert to Long_Long_Integer and use Integer_Formatter
      function As_Integer is new Integer_Formatter (Long_Long_Integer);
   begin
      return As_Integer (Long_Long_Integer (Item), Spec);
   end Modular_Formatter;

   function Float_Formatter
     (Item : Float_Type; Spec : Format_Spec) return String
   is
      use Ada.Strings.Fixed;

      Value : constant Long_Float := Long_Float (Item);

      --  Helper to format with specific precision
      function Fixed_Point_Image
        (V : Long_Float; Precision : Natural) return String
      is
         Abs_Val   : constant Long_Float := abs V;
         Int_Part  : constant Long_Long_Integer :=
           Long_Long_Integer (Long_Float'Floor (Abs_Val));
         Frac_Part : constant Long_Float := Abs_Val - Long_Float (Int_Part);

         --  Build fractional part
         Frac_String : Unbounded_String;
         Scaled      : Long_Float := Frac_Part;
      begin
         if Precision = 0 then
            return Trim (Long_Long_Integer'Image (Int_Part), Ada.Strings.Left);
         end if;

         Append (Frac_String, '.');
         for I in 1 .. Precision loop
            Scaled := Scaled * 10.0;
            declare
               Digit : constant Integer := Integer (Long_Float'Floor (Scaled));
            begin
               Append
                 (Frac_String, Trim (Integer'Image (Digit), Ada.Strings.Left));
               Scaled := Scaled - Long_Float (Digit);
            end;
         end loop;

         return
           Trim (Long_Long_Integer'Image (Int_Part), Ada.Strings.Left)
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
               Mantissa : Long_Float := abs Value;
            begin
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
            --  General format (simplified)
            Append (Result, Fixed_Point_Image (abs Value, Precision));

         when '%' =>
            --  Percentage
            Append (Result, Fixed_Point_Image (abs Value * 100.0, Precision));
            Append (Result, '%');

         when others =>
            --  Fixed point
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
   end Float_Formatter;

   function String_Formatter (Item : String; Spec : Format_Spec) return String
   is
      Result : constant String :=
        (if Spec.Has_Precision and then Spec.Precision < Item'Length
         then Item (Item'First .. Item'First + Spec.Precision - 1)
         else Item);
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
   end String_Formatter;

   function Character_Formatter
     (Item : Character; Spec : Format_Spec) return String is
   begin
      return String_Formatter ((1 => Item), Spec);
   end Character_Formatter;

   function Boolean_Formatter
     (Item : Boolean; Spec : Format_Spec) return String is
   begin
      return String_Formatter ((if Item then "True" else "False"), Spec);
   end Boolean_Formatter;

end Format_Strings.Formatters;

