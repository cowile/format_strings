with Ada.Assertions;
with Ada.Text_IO;
with Format_Strings;

use Ada.Assertions;
use Ada.Text_IO;
use Format_Strings;

procedure Tests is

   procedure Test_Basic_Integer is
   begin
      Put_Line ("Testing basic integer formatting...");
      Assert (Format ("Number: {}", 42) = "Number: 42");
      Assert (Format ("Negative: {}", -42) = "Negative: -42");
      Put_Line ("  Basic integer: PASSED");
   end Test_Basic_Integer;

   procedure Test_Multiple_Args is
   begin
      Put_Line ("Testing multiple arguments...");
      Assert (Format ("x = {}, y = {}", 10, 20) = "x = 10, y = 20");
      Put_Line ("  Multiple args: PASSED");
   end Test_Multiple_Args;

   procedure Test_Positional_Args is
      Result : constant String := Format ("{2} before {1}", 10, 20);
   begin
      Put_Line ("Testing positional arguments...");
      Put_Line ("  Expected: '20 before 10'");
      Put_Line ("  Got:      '" & Result & "'");
      Assert (Result = "20 before 10");
      Put_Line ("  Positional args: PASSED");
   end Test_Positional_Args;

   procedure Test_Width_Formatting is
   begin
      Put_Line ("Testing width formatting...");
      Assert (Format (">{:5}<", 42) = ">   42<");
      Assert (Format (">{:>5}<", 42) = ">   42<");  -- Right is default
      Assert (Format (">{:<5}<", 42) = ">42   <");
      Assert (Format (">{:^5}<", 42) = "> 42  <");
      Put_Line ("  Width formatting: PASSED");
   end Test_Width_Formatting;

   procedure Test_Number_Bases is
   begin
      Put_Line ("Testing number bases...");
      Assert (Format ("Hex: {:#x}", 255) = "Hex: 0xff");
      Assert (Format ("HEX: {:#X}", 255) = "HEX: 0XFF");
      Assert (Format ("Oct: {:#o}", 64) = "Oct: 0100");
      Assert (Format ("Bin: {:#b}", 42) = "Bin: 0b101010");
      Put_Line ("  Number bases: PASSED");
   end Test_Number_Bases;

   procedure Test_Sign_Formatting is
   begin
      Put_Line ("Testing sign formatting...");
      Assert (Format ("{:+}", 42) = "+42");
      Assert (Format ("{:+}", -42) = "-42");
      Put_Line ("  Sign formatting: PASSED");
   end Test_Sign_Formatting;

   procedure Test_String_Formatting is
   begin
      Put_Line ("Testing string formatting...");
      Assert (Format ("Hello, {}!", "World") = "Hello, World!");
      Assert (Format ("{:.3}", "Hello") = "Hel");
      Put_Line ("  String formatting: PASSED");
   end Test_String_Formatting;

   procedure Test_Hole_Counting is
   begin
      Put_Line ("Testing hole counting...");
      Assert (Count_Holes ("") = 0);
      Assert (Count_Holes ("No holes") = 0);
      Assert (Count_Holes ("One {} hole") = 1);
      Assert (Count_Holes ("{} and {}") = 2);
      -- Assert (Count_Holes ("{{escaped}}") = 0);  -- TODO: Handle escapes
      Put_Line ("  Hole counting: PASSED");
   end Test_Hole_Counting;

begin
   Put_Line ("Running Format_Strings v1.0 Tests");
   Put_Line ("=================================");

   Test_Basic_Integer;
   Test_Multiple_Args;
   Test_Positional_Args;
   Test_Width_Formatting;
   Test_Number_Bases;
   Test_Sign_Formatting;
   Test_String_Formatting;
   Test_Hole_Counting;

   New_Line;
   Put_Line ("All tests PASSED!");
end Tests;