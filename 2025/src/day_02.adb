with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

use Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Strings.Unbounded;
use Ada.Text_IO;

procedure Day_02 is
    function Silver_Invalid (N : Big_Integer) return Boolean is
        N_Str   : Unbounded_String;
        Fst_Str : Unbounded_String;
        Snd_Str : Unbounded_String;
    begin
        N_Str := To_Unbounded_String (N'Image);
        N_Str := To_Unbounded_String (Slice (N_Str, 2, Length (N_Str)));

        if Length (N_Str) mod 2 /= 0 then
            return False;
        end if;

        declare
            Half_I : Natural := Length (N_Str) / 2;
        begin
            Fst_Str := To_Unbounded_String (Slice (N_Str, 1, Half_I));
            Snd_Str :=
               To_Unbounded_String (Slice (N_Str, Half_I + 1, Length (N_Str)));
        end;

        return Fst_Str = Snd_Str;
    end;

    function Gold_Invalid_Div
       (N_Str : Unbounded_String; Div : Positive) return Boolean
    is
        N_Len     : constant Positive := Length (N_Str);
        Chunk_Len : constant Positive := N_Len / Div;
        Chunk_I   : Positive;
        Chunk_1   : String := Slice (N_Str, 1, Chunk_Len);
    begin
        if N_Len mod Div /= 0 then
            return False;
        end if;

        for Chunk_I in 2 .. Div loop
            if Chunk_1
               /= Slice
                     (N_Str,
                      (Chunk_I - 1) * Chunk_Len + 1,
                      Chunk_I * Chunk_Len)
            then
                return False;
            end if;
        end loop;

        return True;
    end;

    function Gold_Invalid (N : Big_Integer) return Boolean is
        N_Str : Unbounded_String;
        Div   : Positive;
    begin
        N_Str := To_Unbounded_String (N'Image);
        N_Str := To_Unbounded_String (Slice (N_Str, 2, Length (N_Str)));

        for Div in 2 .. Length (N_Str) loop
            if Gold_Invalid_Div (N_Str, Div) then
                return True;
            end if;
        end loop;

        return False;
    end;

    Silver : Big_Integer := 0;
    Gold   : Big_Integer := 0;

    procedure Count_Range (Lo : Big_Integer; Hi : Big_Integer) is
        N : Big_Integer := 0;
        I : Big_Integer := Lo;
    begin
        while I <= Hi loop
            if Silver_Invalid (I) then
                Silver := Silver + I;
            end if;
            if Gold_Invalid (I) then
                Gold := Gold + I;
            end if;

            I := I + 1;
        end loop;
    end;

    Line       : Unbounded_String;
    Comma_I    : Natural;
    Slash_I    : Natural;
    Prod_Range : Unbounded_String;
    Prod_Lo    : Big_Integer;
    Prod_Hi    : Big_Integer;
begin
    while not End_Of_File loop
        Line := To_Unbounded_String (Get_Line);

        while Length (Line) > 0 loop
            Comma_I := Index (Line, ",");
            if Comma_I = 0 then
                Comma_I := Length (Line);
            end if;

            Prod_Range :=
               To_Unbounded_String
                  (Slice
                      (Line,
                       1,
                       Comma_I - (if Comma_I = Length (Line) then 0 else 1)));
            Line :=
               To_Unbounded_String (Slice (Line, Comma_I + 1, Length (Line)));

            Slash_I := Index (Prod_Range, "-");
            if Slash_I = 0 then
                raise Program_Error with "Invalid product range";
            end if;

            Prod_Lo := From_String (Slice (Prod_Range, 1, Slash_I - 1));
            Prod_Hi :=
               From_String
                  (Slice (Prod_Range, Slash_I + 1, Length (Prod_Range)));

            Count_Range (Prod_Lo, Prod_Hi);
        end loop;
    end loop;

    Put_Line ("Silver: " & Silver'Image);
    Put_Line ("Gold: " & Gold'Image);
end;
