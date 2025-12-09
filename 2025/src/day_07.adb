with Ada.Containers.Vectors;
with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

use Ada.Containers;
use Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Strings.Unbounded;
use Ada.Text_IO;

procedure Day_07 is
    Input_Line : Unbounded_String;
    Input_First : Boolean := True;

    package TC_Vec is new
       Vectors (Index_Type => Positive, Element_Type => Boolean);
    use TC_Vec;

    Tachyons_Classic : TC_Vec.Vector;
    Tachyons_Classic_New : TC_Vec.Vector;

    package TQ_Vec is new
       Vectors (Index_Type => Positive, Element_Type => Big_Integer);
    use TC_Vec;

    Tachyons_Quantum : TQ_Vec.Vector;
    Tachyons_Quantum_New : TQ_Vec.Vector;

    Silver : Big_Integer := 0;
    Gold   : Big_Integer := 0;
begin
    while not End_Of_File loop
        Input_Line := To_Unbounded_String (Get_Line);
        if Input_First then
            Input_First := False;

            for Idx in 1 .. Length(Input_Line) loop
                case Element(Input_Line, Idx) is
                    when 'S' =>
                        Append(Tachyons_Classic, True);
                        TQ_Vec.Append(Tachyons_Quantum, 1);
                    when '.' =>
                        Append(Tachyons_Classic, False);
                        TQ_Vec.Append(Tachyons_Quantum, 0);
                    when others =>
                        raise Constraint_Error;
                end case;
            end loop;
        else
            -- Classic

            Tachyons_Classic_New := Tachyons_Classic;

            for Idx in 1 .. Positive(Length(Tachyons_Classic)) loop
                Tachyons_Classic_New(Idx) := False;
            end loop;

            for Idx in 1 .. Positive(Length(Input_Line)) loop
                if Tachyons_Classic(Idx) then
                    case Element(Input_Line, Idx) is
                        when '^' =>
                            Silver := Silver + 1;
                            if Idx in 2 .. Positive(Length(Tachyons_Classic)) then
                                Tachyons_Classic_New(Idx-1) := True;
                            end if;
                            if Idx in 1 .. Positive(Length(Tachyons_Classic)) - 1 then
                                Tachyons_Classic_New(Idx+1) := True;
                            end if;
                        when '.' =>
                            Tachyons_Classic_New(Idx) := True;
                        when others =>
                            raise Constraint_Error;
                    end case;
                end if;
            end loop;

            Tachyons_Classic := Tachyons_Classic_New;

            -- Quantum

            Tachyons_Quantum_New := Tachyons_Quantum;

            for Idx in 1 .. Positive(TQ_Vec.Length(Tachyons_Quantum)) loop
                Tachyons_Quantum_New(Idx) := 0;
            end loop;

            for Idx in 1 .. Positive(Length(Input_Line)) loop
                if Tachyons_Quantum(Idx) > 0 then
                    case Element(Input_Line, Idx) is
                        when '^' =>
                            if Idx in 2 .. Positive(TQ_Vec.Length(Tachyons_Quantum)) then
                                Tachyons_Quantum_New(Idx-1) := Tachyons_Quantum_New(Idx-1) + Tachyons_Quantum(Idx);
                            end if;
                            if Idx in 1 .. Positive(TQ_Vec.Length(Tachyons_Quantum)) - 1 then
                                Tachyons_Quantum_New(Idx+1) := Tachyons_Quantum_New(Idx+1) + Tachyons_Quantum(Idx);
                            end if;
                        when '.' =>
                            Tachyons_Quantum_New(Idx) := Tachyons_Quantum_New(Idx) + Tachyons_Quantum(Idx);
                        when others =>
                            raise Constraint_Error;
                    end case;
                end if;
            end loop;

            Tachyons_Quantum := Tachyons_Quantum_New;
        end if;
    end loop;

    Gold := 0;
    for Idx in 1 .. Positive(TQ_Vec.Length(Tachyons_Quantum)) loop
        Gold := Gold + Tachyons_Quantum(Idx);
    end loop;

    Put_Line ("Silver: " & Silver'Image);
    Put_Line ("Gold: " & Gold'Image);
end;
