"""
A simple parser for latex expressions (as an AST).

Produces a series of "LatexExpr"
"""
import re
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from decimal import Decimal


def janetString(s):
    res = ['"']
    for c in s:
        # Outside of printable ASCII range must be escaped
        if ord(c) not in range(32, 177):
            res.append(f"\\x{b:x02}" for b in c.encode("utf8"))
            continue
        # Special case some printable ascii chars
        match c:
            case "\\" | '"':
                res.append("\\")
                res.append(c)
            case "\n":
                res.append("\\n")
            case "\t":
                res.append("\\t")
            case _:
                res.append(c)
    res.append('"')
    return "".join(res)


SIMPLE_KEYWORD_PATTERN = re.compile("[\w_-]+")


def janetKeyword(name):
    if SIMPLE_KEYWORD_PATTERN.fullmatch(name):
        return ":" + name
    else:
        return f"(keyword {janetString(name)})"


@dataclass(slots=True)
class LatexExpr(metaclass=ABCMeta):
    """A latex expression"""

    def __str__(self):
        return self.sexpr()

    @abstractmethod
    def latex(self) -> str:
        """Prints as a latex expression"""
        pass

    @abstractmethod
    def sexpr(self) -> str:
        """Prints as a s-expression (Janet-style)"""
        pass


@dataclass(init=False, slots=True)
class LatexSeq(LatexExpr):
    """A sequence of latex expressions"""

    children: list[LatexExpr]

    def __init__(self, children: list[LatexExpr]):
        match len(children):
            case 0:
                raise AssertionError("Empty list")
            case 1:
                raise AssertionError("List of length one: " + children[1])
            case _:
                self.children = children

    def latex(self) -> str:
        return " ".join(child.latex() for child in self.children)

    def sexpr(self) -> str:
        return "[" + " ".join(child.sexpr() for child in self.children) + "]"


@dataclass(slots=True)
class LatexCommand(LatexExpr):
    command_name: str
    args: list[LatexExpr]

    def latex(self) -> str:
        res = [f"\\{self.command}"]
        if self.args:
            res.append("{")
            res.append(", ".join(arg.latex() for arg in self.args))
            res.append("}")
        return "".join(res)

    def sexpr(self) -> str:
        res = ["latex", janetKeyword(self.command_name)]
        for arg in self.args:
            res.append(arg.sexpr())
        return "(" + " ".join(res) + ")"


@dataclass(slots=True)
class LatexLiteral(LatexExpr):
    text: str

    def latex(self) -> str:
        return self.text

    def sexpr(self) -> str:
        try:
            return int(self.text)
        except ValueError:
            pass
        try:
            return Decimal(self.text)
        except ValueError:
            pass
        return janetString(self.text)
