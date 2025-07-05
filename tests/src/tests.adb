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

   procedure Test_Zero_Padding is
   begin
      Put_Line ("Testing zero padding...");
      
      -- Basic zero padding
      Assert (Format ("{:05}", 42) = "00042");
      Assert (Format ("{:05}", -42) = "-0042");
      
      -- Zero padding with different bases
      Assert (Format ("{:#010x}", 255) = "0x000000ff");
      Assert (Format ("{:#010X}", 255) = "0X000000FF");
      Assert (Format ("{:#08b}", 15) = "0b001111");
      
      -- Zero padding with sign
      Assert (Format ("{:+05}", 42) = "+0042");
      Assert (Format ("{:+05}", -42) = "-0042");
      
      -- Zero padding is ignored with explicit alignment
      Assert (Format ("{:<05}", 42) = "42   ");
      Assert (Format ("{:>05}", 42) = "   42");
      
      Put_Line ("  Zero padding: PASSED");
   end Test_Zero_Padding;

   procedure Test_Float_Formatting is
   begin
      Put_Line ("Testing float formatting...");
      
      -- Basic float formatting
      Assert (Format ("Pi: {}", 3.14159) = "Pi: 3.141590");
      
      -- Precision control
      Assert (Format ("Pi: {:.2f}", 3.14159) = "Pi: 3.14");
      Assert (Format ("Pi: {:.4f}", 3.14159) = "Pi: 3.1415");
      Assert (Format ("Int: {:.0f}", 3.14159) = "Int: 3");
      
      -- Sign control
      Assert (Format ("{:+.2f}", 3.14) = "+3.14");
      Assert (Format ("{:+.2f}", -3.14) = "-3.14");
      
      -- Scientific notation
      Assert (Format ("{:.2e}", 1234.5) = "1.23e+3");
      Assert (Format ("{:.2E}", 1234.5) = "1.23E+3");
      Assert (Format ("{:.3e}", 0.00123) = "1.230e-3");
      
      -- Percentage
      Assert (Format ("{:.1%}", 0.125) = "12.5%");
      Assert (Format ("{:.0%}", 0.99) = "99%");
      
      -- Width and alignment
      Assert (Format ("{:10.2f}", 3.14) = "      3.14");
      Assert (Format ("{:<10.2f}", 3.14) = "3.14      ");
      Assert (Format ("{:^10.2f}", 3.14) = "   3.14   ");
      
      Put_Line ("  Float formatting: PASSED");
   end Test_Float_Formatting;

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
   Test_Zero_Padding;
   Test_Float_Formatting;

   New_Line;
   Put_Line ("All tests PASSED!");
end Tests;