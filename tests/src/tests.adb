with Ada.Assertions;
with Format_Strings;

use Ada.Assertions;
use Format_Strings;

procedure Tests is
   procedure Format_Int  is new Format (Integer);

   procedure Empty is
      F : constant Format_String := Create ("");
      S : constant String := To_String (F);
   begin
      Assert (S = "");
   end Empty;

   procedure Const is
      F : constant Format_String := Create ("abcdefg");
      S : constant String := To_String (F);
   begin
      Assert (S = "abcdefg");
   end Const;

   I : constant Integer := 10101;

   procedure Lone_Hole is
      F : Format_String := Create ("{}");
   begin
      Format_Int (F, I);
      Assert (To_String (F) = Integer'Image (I));
   end Lone_Hole;

   procedure Lead_Text is
      F : Format_String := Create ("My Int: {}");
   begin
      Format_Int (F, I);
      Assert (To_String (F) = "My Int: " & Integer'Image (I));
   end Lead_Text;

   procedure Trail_Text is
      F : Format_String := Create ("{} is my int.");
   begin
      Format_Int (F, I);
      Assert (To_String (F) = Integer'Image (I) & " is my int.");
   end Trail_Text;

   procedure Text is
      F : Format_String := Create ("My Int: {} is my int.");
   begin
      Format_Int (F, I);
      Assert (To_String (F) = "My Int: " & Integer'Image (I) & " is my int.");
   end Text;

   procedure Multi is
      F : Format_String := Create ("Testing... {} sucks and {} rocks. {}");
   begin
      Format_Int (F, 1);
      Format_Int (F, 2);
      Format_Int (F, 3);
      Assert (To_String (F) = "Testing...  1 sucks and  2 rocks.  3");
   end Multi;

begin
   Empty;
   Const;
   Lone_Hole;
   Lead_Text;
   Trail_Text;
   Text;
   Multi;
end Tests;
