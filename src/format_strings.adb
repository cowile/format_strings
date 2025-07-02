package body Format_Strings is

   type State is (Normal, Escape, Start_Hole);

   function Count_Holes (T : Template) return Natural is
      S : State := Normal;
      C : Character;
      Count : Natural := 0;
   begin
      for I in 1 .. Bounded_Strings.Length (T)
      loop
         C := Bounded_Strings.Element (T, I);
         if S = Escape then
            S := Normal;
         elsif C = Escape_Character then
            S := Escape;
         elsif S = Start_Hole and then C = End_Hole_Character then
            Count := Count + 1;
            S := Normal;
         elsif C = Start_Hole_Character then
            S := Start_Hole;
         else
            S := Normal;
         end if;
      end loop;

      return Count;
   end Count_Holes;

   function Create (T : Template) return Format_String is
      Holes : constant Natural := Count_Holes (T);
      Parts : constant Natural := 2 * Holes;
      To_Format :  Format_String :=
        (Parts => Parts, Given => T, others => <>);
      Pos : Natural := 0;
      S : State := Normal;
      C : Character;
   begin
      for I in 1 .. Bounded_Strings.Length (T)
      loop
         C := Bounded_Strings.Element (T, I);
         if S = Escape then
            S := Normal;
            Bounded_Strings.Append (To_Format.Constructed (Pos), C);
         elsif C = Escape_Character then
            S := Escape;
         elsif S = Start_Hole and then C = End_Hole_Character then
            S := Normal;
            Pos := Pos + 2;
         elsif C = Start_Hole_Character then
            S := Start_Hole;
         else
            S := Normal;
            Bounded_Strings.Append (To_Format.Constructed (Pos), C);
         end if;
      end loop;

      return To_Format;
   end Create;

   function Create (S : String) return Format_String is
      T : Template := Bounded_Strings.To_Bounded_String (S);
   begin
      return Create (T);
   end Create;

   procedure Format (FS : in out Format_String; Arg : T) is
      Pos : constant Natural := FS.Filled * 2 + 1;
   begin
      Bounded_Strings.Append (FS.Constructed (Pos), T'Image (Arg));
      FS.Filled := FS.Filled + 1;
   end Format;

   function To_String (FS : Format_String) return String is
      Formatted : Bounded_Strings.Bounded_String;
   begin
      for S of FS.Constructed loop
         Bounded_Strings.Append (Formatted, S);
      end loop;

      return Bounded_Strings.To_String (Formatted);
   end To_String;

end Format_Strings;
