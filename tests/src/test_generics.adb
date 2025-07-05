with Ada.Assertions;
with Ada.Text_IO;
with Format_Strings;
with Format_Strings.Formatters;

use Ada.Assertions;
use Ada.Text_IO;
use Format_Strings;

procedure Test_Generics is
   
   use Format_Strings.Common;  -- Use pre-instantiated combinations
   
   --  Instantiate formatters
   function Int_Formatter is new Formatters.Integer_Formatter (Integer);
   function Float_Formatter is new Formatters.Float_Formatter (Float);

   --  Instantiate generic Format functions
   function Format_I is new Format (Integer, Int_Formatter);
   function Format_F is new Format (Float, Float_Formatter);
   function Format_S (Template : String; Arg : String) return String
      renames Format_Strings.Format_Str;
   function Format_B is new Format (Boolean, Formatters.Boolean_Formatter);

   --  Two-argument formats
   function Format_II is new Format_2 (Integer, Integer, Int_Formatter, Int_Formatter);
   function Format_SI (Template : String; Arg1 : String; Arg2 : Integer) return String is
      function F2 is new Format_2 (String, Integer, Formatters.String_Formatter, Int_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format_SI;
   
   --  Three-argument format
   function Format_III is new Format_3 (Integer, Integer, Integer, 
                                        Int_Formatter, Int_Formatter, Int_Formatter);
   
   procedure Test_Generic_Integer is
   begin
      Put_Line ("Testing generic integer formatting...");
      Assert (Format_I ("Number: {}", 42) = "Number: 42");
      Assert (Format_I ("Hex: {:#x}", 255) = "Hex: 0xff");
      Put_Line ("  Generic integer: PASSED");
   end Test_Generic_Integer;
   
   procedure Test_Generic_Float is
   begin
      Put_Line ("Testing generic float formatting...");
      Assert (Format_F ("Pi: {:.2f}", 3.14159) = "Pi: 3.14");
      Assert (Format_F ("{:.2e}", 1234.5) = "1.23e+3");
      Put_Line ("  Generic float: PASSED");
   end Test_Generic_Float;
   
   procedure Test_Generic_String is
   begin
      Put_Line ("Testing generic string formatting...");
      Assert (Format_S ("Hello, {}!", "World") = "Hello, World!");
      Assert (Format_S ("{:>10}", "test") = "      test");
      Put_Line ("  Generic string: PASSED");
   end Test_Generic_String;
   
   procedure Test_Generic_Boolean is
   begin
      Put_Line ("Testing generic boolean formatting...");
      Assert (Format_B ("Value: {}", True) = "Value: True");
      Assert (Format_B ("Value: {}", False) = "Value: False");
      Put_Line ("  Generic boolean: PASSED");
   end Test_Generic_Boolean;
   
   procedure Test_Mixed_Types is
   begin
      Put_Line ("Testing mixed type formatting...");
      
      -- Using pre-instantiated functions from Common
      Assert (Format_SI ("{} = {}", "The answer", 42) = "The answer = 42");
      Assert (Format_IS ("User ID: {}, Name: {}", 123, "Alice") = "User ID: 123, Name: Alice");
      Assert (Format_IF ("Count: {}, Average: {:.2f}", 10, 3.14159) = "Count: 10, Average: 3.14");
      
      -- Three arguments
      Assert (Format_SII ("{} scored {} out of {}", "Bob", 85, 100) = "Bob scored 85 out of 100");
      Assert (Format_ISI ("Player {} - {} - Score: {}", 1, "Charlie", 9500) = "Player 1 - Charlie - Score: 9500");
      
      -- Four arguments
      Assert (Format_SSII ("{} {} earned {} points in {} games", 
                           "Team", "Alpha", 250, 5) = "Team Alpha earned 250 points in 5 games");
      
      -- Five arguments with mixed types
      Assert (Format_SISIB ("User {} (ID: {}) from {} has {} messages. Active: {}",
                            "Dave", 456, "NYC", 12, True) = 
              "User Dave (ID: 456) from NYC has 12 messages. Active: True");
      
      -- Positional arguments
      Assert (Format_SI ("{1} = {2}", "The answer", 42) = "The answer = 42");
      Assert (Format_SI ("{2} is the answer to {1}", "everything", 42) = "42 is the answer to everything");
      
      Put_Line ("  Mixed types: PASSED");
   end Test_Mixed_Types;
   
   procedure Test_Custom_Type is
      type RGB_Color is record
         R, G, B : Natural range 0 .. 255;
      end record;
      
      function RGB_Formatter (Item : RGB_Color; Spec : Format_Spec) return String is
      begin
         if Spec.Type_Char = 'h' then
            --  Hex format
            return Format_I ("#{:02x}", Item.R) &
                   Format_I ("{:02x}", Item.G) &
                   Format_I ("{:02x}", Item.B);
         else
            --  Default format
            return Format_III ("rgb({}, {}, {})", Item.R, Item.G, Item.B);
         end if;
      end RGB_Formatter;
      
      function Format_RGB is new Format (RGB_Color, RGB_Formatter);
      
      Color : constant RGB_Color := (R => 255, G => 128, B => 0);
   begin
      Put_Line ("Testing custom type formatting...");
      Assert (Format_RGB ("Color: {}", Color) = "Color: rgb(255, 128, 0)");
      Assert (Format_RGB ("Color: {:h}", Color) = "Color: #ff8000");
      Put_Line ("  Custom type: PASSED");
   end Test_Custom_Type;

begin
   Put_Line ("Running Generic Format_Strings Tests");
   Put_Line ("===================================");
   
   Test_Generic_Integer;
   Test_Generic_Float;
   Test_Generic_String;
   Test_Generic_Boolean;
   Test_Mixed_Types;
   Test_Custom_Type;
   
   New_Line;
   Put_Line ("All generic tests PASSED!");
end Test_Generics;
