from typing import Optional


class PhaseType:
    UNKNOWN = 0
    BASAL = 1
    STIM = 2

    def from_str(t: str, stim_label: str) -> int:
        match t.upper():
            case "BASAL":
                return PhaseType.BASAL
            case stim_label.upper():
                return PhaseType.STIM
            case _:
                return PhaseType.UNKNOWN

    def from_int(i: int) -> str:
        match i:
            case PhaseType.UNKNOWN:
                return "Unknown"
            case PhaseType.BASAL:
                return "Basal"
            case PhaseType.STIM:
                return "Stimulation"


class ConvertingValues:
    """
    Return value for a converting rule. It is the result of the parsing
    of a filename.
    Any converting rule should take a filename as a string as a parameter,
    and an optional string for matching the stimulation label patter and
    return a ConvertingValues. A converting rule can fail but should not
    handle the exception itself. It's the user of the rule that is
    responsible to handle it. The definition of a converting rule should be:

    def converting_rule(
        filename: str, stim_label: Optional[str] = None
    ) -> ConvertingValues:
        ...

    Its members are:
    - matrice: str          the identifier of the batch
    - cond:    str          the condition of stimulation (i.e. 50 as a value of ultrasound)
    - div:     int          the DIV of the batch
    - i:       int          the position of the
    - t:       PhaseType    the type of the phase (basal, stimulation)
    - s_cond   str          the additional info about the stimulation
    """

    def __init__(self, matrice: str, cond: str, div: int, i: int, t: int, stim_cond: str = ""):
        self.matrice = matrice
        self.cond = cond
        self.div = div
        self.i = int(i)
        self.t = t
        self.s_cond = stim_cond

    def __str__(self) -> str:
        return f"""{{
        matrice: {self.matrice},
        cond: {self.cond},
        div: {self.div},
        i: {self.i},
        t: {PhaseType.from_int(self.t)},
        stim_cond = {self.s_cond},
}}"""

    
def rule_order_type_cond(name: str, matrix_name: str, cond: str, div: int) -> ConvertingValues:
    """
    You may apply this rule with a lambda with the matrix, cond and div values.
    Example: 0001_basale.h5
    rule = lambda: rule1(name, "12345", "100E", 77)
    {
        matrix: 12345,
        cond: 100E,
        div: 77,
        i: 01,
        t: PhaseType.BASAL,
    }

    Example: 0002_US_50.h5
    rule = lambda: rule1(name, "12345", "100E", 77)
    {
        matrix: 12345,
        cond: 100E,
        div: 77,
        i: 02,
        t: PhaseType.STIM,
    }
    """
    first_ = name.find("_")
    i = name[: first_]
    second_ = name.find("_", first_ + 1)
    global t
    if second_ > 0:
        t = name[first_ + 1: second_]
        s_cond = name[second_ + 1:-3]
    else:
        t = name[first_ + 1: second_-2]
        s_cond = ""
    match t.upper():
        case "BASALE":
            t = PhaseType.BASAL
        case "US":
            t = PhaseType.STIM
        case _:
            t = PhaseType.UNKNOWN
    return ConvertingValues(matrice=matrix_name, cond=cond, div=div, i=i, t=t, stim_cond=s_cond)


def rule1(name: str, matrix_name: str, cond: str, div: int) -> ConvertingValues:
    """
    You may apply this rule with a lambda with the matrix, cond and div values.
    Example: 01_basal.h5
    rule = lambda: rule1(name, "12345", "100E", 77)
    {
        matrix: 12345,
        cond: 100E,
        div: 77,
        i: 01,
        t: basal,
    }
    """
    i = name[: name.find("_")]
    t = name[name.find("_") + 1 : -3]
    return ConvertingValues(
        matrix_name, cond, f"{div}", f"000{i}", PhaseType.from_str(t, "US")
    )


def rule2(name: str) -> ConvertingValues:
    """
    Example: 2024-04-11T14-31-1938940_100E_DIV77_nbasal_0001_E-00155.h5
    {
        matrix: 38940,
        cond: 100E,
        div: 77,
        i: 01,
        t: nbasal,
    }
    """
    try:
        first_ = name.find("_") + 1
        matrice = name[first_ - 6 : first_ - 1]
        second_ = name.find("_", first_) + 1
        cond = name[first_ : second_ - 1]
        third_ = name.find("_", second_) + 1
        div = name[name.find("DIV") + 3 : third_ - 1]
        fourth_ = name.find("_", third_) + 1
        t = name[third_ : fourth_ - 1]
        fifth_ = name.find("_", fourth_) + 1
        i = str(int(name[fourth_ : fifth_ - 1]))
        return ConvertingValues(matrice, cond, div, f"000{i}", t)
    except Exception as e:
        print(e)
        return None


def rule3(name: str) -> Optional[ConvertingValues]:
    """
    Example: 38940_100E_DIV77_nbasal_0001.h5
    {
        matrix: 38940,
        cond: 100E,
        div: 77,
        i: 01,
        t: nbasal,
    }
    """
    try:
        first_ = name.find("_") + 1
        matrice = name[: first_ - 1]
        second_ = name.find("_", first_) + 1
        cond = name[first_ : second_ - 1]
        third_ = name.find("_", second_) + 1
        div = name[name.find("DIV") + 3 : third_ - 1]
        fourth_ = name.find("_", third_) + 1
        t = name[third_ : fourth_ - 1]
        fifth_ = name.find("_", fourth_) + 1
        if fifth_ > 0:
            i = str(int(name[fifth_:-3]))
        else:
            i = str(int(name[fourth_:-3]))
        return ConvertingValues(matrice, cond, div, f"000{i}", t)
    except Exception as e:
        print(e)
        return None
