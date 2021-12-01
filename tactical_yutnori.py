from collections import defaultdict
from enum import Enum
import random



# CONFIG
class StickType(Enum):

    U = UNMARKED = 'U'
    M = MARKED   = 'M'


class _StickComb(type):

    def __init__(cls, *args, **kwargs):
        M = StickType.MARKED
        U = StickType.UNMARKED

        # pairs
        cls.MM = cls([M, M])
        cls.UU = cls([U, U])
        cls.MU = cls([M, U])

        # aliases
        cls.MARKED_MARKED = cls.MM
        cls.UNMARKED_UNMARKED = cls.UU
        cls.MARKED_UNMARKED = cls.UNMARKED_MARKED = cls.UM = cls.MU

        # quads
        cls.BACK_DO = cls([U, M, M, M])
        cls.GAE = cls([U, U, M, M])
        cls.GEOL = cls([U, U, U, M])
        cls.YUT = cls([U, U, U, U])
        cls.MO = cls([M, M, M, M])

        cls.OUTCOMES = (cls.BACK_DO, cls.GAE, cls.GEOL, cls.YUT, cls.MO)


class StickComb(metaclass=_StickComb):

    def __init__(self, seq):
        self._n_unmarked = seq.count(StickType.U)
        self._n_marked = len(seq) - self._n_unmarked
        assert self._n_marked == seq.count(StickType.M)

    def __iter__(self):
        return iter(self.seq)

    def __hash__(self):
        return hash((self._n_unmarked, self._n_marked))

    def __add__(self, other):
        if isinstance(other, self.__class__):
            seq = [StickType.U] * (self.n_U + other.n_U) + \
                  [StickType.M] * (self.n_M + other.n_M)
            return StickComb(seq)
        raise TypeError(f'{self.__class__.__name__} cannot be added with object of type {type(other)}.')

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            return (self.n_U == other.n_U and self.n_M == other.n_M)
        return False

    def __str__(self):
        return '(' + ' '.join([str(i) for i in self.seq]) + ')'

    def __repr__(self):
        seq_repr = ' '.join([repr(i) for i in self.seq])
        return f'{self.__class__.__name__}({seq_repr})'

    @property
    def seq(self):
        for _ in range(self._n_unmarked):
            yield StickType.U
        for _ in range(self._n_marked):
            yield StickType.M

    @property
    def name(self):
        if (self.n_U + self.n_M) == 4:
            if self == self.BACK_DO:
                return 'BACK-DO'
            elif self == self.GAE:
                return 'GAE'
            elif self == self.GEOL:
                return 'GEOL'
            elif self == self.YUT:
                return 'YUT'
            elif self == self.MO:
                return 'MO'
        return NotImplemented

    @property
    def n_unmarked(self):
        return self._n_unmarked

    @property
    def n_marked(self):
        return self._n_marked

    @property
    def n_U(self):
        return self._n_unmarked

    @property
    def n_M(self):
        return self._n_marked


class BoardNode(object):

    SPECIAL = Enum('SPECIAL', dict(OUT='OUT', WON='WON'))
    CORNERS = Enum('CORNERS', dict(SE='SE', NE='NE', CC='CC', NW='NW', SW='SW'))

    EAST = Enum('EAST', dict(E1='E1', E2='E2', E3='E3', E4='E4'))
    NORTH = Enum('NORTH', dict(N1='N1', N2='N2', N3='N3', N4='N4'))
    WEST = Enum('WEST', dict(W1='W1', W2='W2', W3='W3', W4='W4'))
    SOUTH = Enum('SOUTH', dict(S1='S1', S2='S2', S3='S3', S4='S4'))

    NORTH_EAST = Enum('NORTH_EAST', dict(NE1='NE1', NE2='NE2', NE4='NE4', NE5='NE5'))
    NORTH_WEST = Enum('NORTH_WEST', dict(NW1='NW1', NW2='NW2', NW4='NW4', NW5='NW5'))


class BoardGraph(object):

    OUT, WON = BoardNode.SPECIAL
    SE, NE, CC, NW, SW = BoardNode.CORNERS

    E1, E2, E3, E4 = BoardNode.EAST
    N1, N2, N3, N4 = BoardNode.NORTH
    W1, W2, W3, W4 = BoardNode.WEST
    S1, S2, S3, S4 = BoardNode.SOUTH

    NE1, NE2, NE4, NE5 = BoardNode.NORTH_EAST
    NW1, NW2, NW4, NW5 = BoardNode.NORTH_WEST

    LAYOUT = (
        (NW, N4, N3,  N2, N1, NE),
        (W1, NW1,        NE1, E4),
        (W2, NW2,        NE2, E3),
        (           CC,         ),
        (W3, NE4,        NW4, E2),
        (W4, NE5,        NW5, E1),
        (SW, S1, S2,  S3, S4, SE),
    )

    _OUTCOMES = (
        StickComb.BACK_DO,
        StickComb.GAE,
        StickComb.GEOL,
        StickComb.YUT,
        StickComb.MO
    )

    _TRANSITIONS = {
        # from_node: (dest_node | outcome for outcome in _OUTCOMES)
        # _OUTCOMES order is assumed and respected for all _TRANSITIONS!

        # No piece on board
        OUT: (OUT, E2, E3, E4, NE),

        # center and corners
        NE: (E4, (N2, NE2), (N3, CC), (N4, NE4), (NW, NE5)),
        NW: (N4, (W2, NW2), (W3, CC), (W4, NW4), (SW, NW5)),
        SW: (W4, S2, S3, S4, SE),
        SE: ((S4, NW5), WON, WON, WON, WON),
        CC: ((NW2, NE2), (NW5, NE5), (SW, SE), (S1, WON), (S2, WON)),

        # north-east diagonal
        NE1: (NE, CC, NE4, NE5, SW),
        NE2: (NE1, NE4, NE5, SW, S1),
        NE4: (CC, NE5, SW, S1, S2),
        NE5: (NE4, S1, S2, S3, S4),

        # north-west diagonal
        NW1: (NW, CC, NW4, NW5, SE),
        NW2: (NW1, NW4, NW5, SE, WON),
        NW4: (CC, NW5, WON, WON, WON),
        NW5: (NW4, WON, WON, WON, WON),

        # straight-line clockwise 1s
        E1: (WON, E3, E4, NE, N1),
        N1: (NE, N3, N4, NW, W1),
        W1: (NW, W3, W4, SW, S1),
        S1: (SW, S3, S4, SE, WON),

        # straight-line clockwise 2s
        E2: (E1, E4, NE, N1, N2),
        N2: (N1, N4, NW, W1, W2),
        W2: (W1, W4, SW, S1, S2),
        S2: (S1, S4, SE, WON, WON),

        # straight-line clockwise 3s
        E3: (E2, NE, N1, N2, N3),
        N3: (N2, NW, W1, W2, W3),
        W3: (W2, SW, S1, S2, S3),
        S3: (S2, SE, WON, WON, WON),

        # straight-line clockwise 4s
        E4: (E3, N1, N2, N3, N4),
        N4: (N3, W1, W2, W3, W4),
        W4: (W3, S1, S2, S3, S4),
        S4: (S3, WON, WON, WON, WON),
    }

    @classmethod
    def transition(cls, orig_node, outcome):
        i = cls._OUTCOMES.index(outcome)
        dest_nodes = cls._TRANSITIONS[orig_node][i]
        return dest_nodes



# GAME
class TacticalYutNori:
    '''
    Tactical Yut-Nori
    - special rules from The Genius (korean tv show)
    - adaptation for two-players version: all four pieces required to win
    '''

    N_PIECES_PER_PLAYER = 4
    N_PIECES_REQ_TO_WIN = 4

    graph = BoardGraph
    NODE_OUT = BoardNode.SPECIAL.OUT
    NODE_WON = BoardNode.SPECIAL.WON

    OUTCOMES = (BACK_DO, GAE, GEOL, YUT, MO) = (
        StickComb.BACK_DO,
        StickComb.GAE,
        StickComb.GEOL,
        StickComb.YUT,
        StickComb.MO
    )

    @property
    def obs(self):
        return NotImplemented

    def reset(self):
        self.players = (+1, -1)

        self.board = {}  # node  : pieces (occupant)
        self.where = {}  # piece : node   (node-key)

        pieces_values = range(1, self.N_PIECES_PER_PLAYER + 1)

        self.board[self.NODE_WON] = set()
        self.board[self.NODE_OUT] = set().union(
            {+piece for piece in pieces_values},
            {-piece for piece in pieces_values},
        )

        for piece in pieces_values:
            self.where[+piece] = self.NODE_OUT
            self.where[-piece] = self.NODE_OUT

        self.turn = 0
        self.history = {}
        self.cur_player = self.players[self.turn]

        return self.obs

    def all_pieces_and_nodes(self):
        return self.where.items()

    def all_pieces(self):
        return self.where.keys()

    def owner(self, piece):
        same_sign_of_player0 = (int(piece > 0) == int(self.players[0] > 0))
        if same_sign_of_player0:
            return self.players[0]
        else:
            return self.players[1]

    def check_winner(self):
        pieces_won = self.board[self.NODE_WON]
        win_thresh = self.N_PIECES_REQ_TO_WIN

        if len(pieces_won) >= win_thresh:
            pieces = defaultdict(set)
            for piece in self.board[self.NODE_WON]:
                pieces[self.owner(piece)].add(piece)
            for player in self.players:
                if len(pieces[player]) >= win_thresh:
                    return player

        return None

    def toss(self, stick_comb):
        outcome = stick_comb

        if outcome not in self.OUTCOMES:
            raise ValueError(f'Outcome {outcome!r} not recognized.\n'
                             f'Accepted outcomes = {self.OUTCOMES!r}')

        if self.sticks_been_tossed():
            raise ValueError('Sticks have already been tossed this turn!')

        self.history[self.turn] = {
            'cur_player': self.cur_player,
            'outcome'   : outcome,
        }

        self._try_infer_orig_node()
        self._try_infer_dest_node()

    def _try_infer_orig_node(self):
        # If only one, among the player's pieces, is in board and outcome was
        # a back-do, the player is forced to move that only piece / set.
        # NB: If none on the board, the player essentially skips, the selection
        # is however still encoded as a placement (orig_node = NODE_OUT).
        orig_nodes = self.valid_orig_nodes_for_selection()

        if len(orig_nodes) == 1:
            node, = orig_nodes
            self.history[self.turn]['orig_node'] = node
            return True

        elif len(orig_nodes) == 0:  # back-do without pieces in board
            self.history[self.turn]['orig_node'] = self.NODE_OUT
            return True

        return False

    def _try_infer_dest_node(self):
        if self.orig_node_been_selected():
            dest_nodes = self.valid_dest_nodes_for_selection()

            if len(dest_nodes) == 1:
                node, = dest_nodes
                self.history[self.turn]['dest_node'] = node
                return True

        return False

    def sticks_been_tossed(self):
        return (self.turn in self.history)

    def get_toss_outcome(self):
        return self.history[self.turn]['outcome']

    def select_orig_node(self, node):
        if not self.sticks_been_tossed():
            raise ValueError('Sticks have not been tossed yet!')

        if node not in self.valid_orig_nodes_for_selection():
            raise ValueError(f'Invalid origin node {node} selected!\n'
                             f'Valid origins (nodes) = '
                             f'{self.valid_orig_nodes_for_selection()}')

        self.history[self.turn]['orig_node'] = node

        # dest depends on orig
        self.history[self.turn].pop('valid_dest_nodes', None)
        self.history[self.turn].pop('dest_node', None)

    def orig_node_been_selected(self):
        return (self.history[self.turn].get('orig_node') is not None)

    def get_orig_node_selected(self):
        return self.history[self.turn]['orig_node']

    def valid_orig_nodes_for_selection(self):
        if not self.sticks_been_tossed():
            raise ValueError('Sticks have not been tossed yet!')

        key = 'valid_orig_nodes'

        # set cache
        if key not in self.history[self.turn]:
            orig_nodes = {node for piece, node in self.where.items()
                           if (self.owner(piece) == self.cur_player
                           and node != self.NODE_WON)}
            if self.get_toss_outcome() == self.BACK_DO:
                orig_nodes.discard(self.NODE_OUT)
            self.history[self.turn][key] = frozenset(orig_nodes)

        # retrieve cache
        return self.history[self.turn][key]

    def select_dest_node(self, node):
        if not self.sticks_been_tossed():
            raise ValueError('Sticks have not been tossed yet!')

        if not self.orig_node_been_selected():
            raise ValueError('Piece to move/place not selected yet!')

        if node not in self.valid_dest_nodes_for_selection():
            raise ValueError(f'Invalid destination node {node} selected!\n'
                             f'Valid destinations (nodes) = '
                             f'{self.valid_dest_nodes_for_selection()}')

        self.history[self.turn]['dest_node'] = node

    def dest_node_been_selected(self):
        return (self.history[self.turn].get('dest_node') is not None)

    def get_dest_node_selected(self):
        return self.history[self.turn]['dest_node']

    def valid_dest_nodes_for_selection(self):
        if not self.sticks_been_tossed():
            raise ValueError('Sticks have not been tossed yet!')

        key = 'valid_dest_nodes'

        # set cache
        if key not in self.history[self.turn]:
            s = orig_node = self.get_orig_node_selected()
            a = outcome = self.get_toss_outcome()
            s1 = self.graph.transition(s, a)  # s -> s1
            dest_nodes = (s1,) if not isinstance(s1, (tuple, list)) else s1
            self.history[self.turn][key] = frozenset(dest_nodes)

        # retrieve cache
        return self.history[self.turn][key]

    def step(self):
        if self.dest_node_been_selected():
            transition = (self.get_orig_node_selected(),
                          self.get_dest_node_selected())
            return self._step(transition)
        return False

    def _step(self, transition):
        origin, dest = transition  # state nodes transition

        capture = False
        if origin != self.NODE_OUT:
            capture = self._move(origin, dest)
        elif dest != self.NODE_OUT:
            capture = self._place(dest)
        # else:
        # back-do does not allow piece placement and, on an empty board,
        # since there are no pieces to move, it results in a 'skip turn'

        # capturing enemy pieces or tossing a YUT / MO results in a bonus turn
        if capture or self.get_toss_outcome() in (self.YUT, self.MO):
            self.history[self.turn]['bonus_turn'] = True
        else:
            self.cur_player = -self.cur_player

        self._next_turn()

        obs = self.obs
        winner = self.check_winner()
        reward = winner if winner else 0
        done = bool(winner)
        info = {}

        return obs, reward, done, info

    def _place(self, dest):
        # a new piece is placed at dest
        OUT, WON = self.NODE_OUT, self.NODE_WON

        if dest == WON:  # can only move there, not place!
            raise ValueError(f'attempting to direclty place a piece to won area!')
        if dest == OUT:  # OUT to OUT is valid transition: simply return no-capture
            # since nothing has to change it's just faster to avoid unneeded checks
            # but the below code would work just fine. Can only occur back-do with
            # no pieces on board, if origin is encoded as OUT.
            return False

        owner = self.cur_player  # +1 / -1
        piece = next((p for p in self.board[OUT] if self.owner(p) == owner)) # 1st unused
        others = self.board.setdefault(dest, set())  # are there pieces at dest?
        capture = bool(len(others) and self.owner(next(iter(others))) != owner)

        if capture:
            for piece_captured in others:
                self.where[piece_captured] = OUT
            self.board[OUT].update(others)
            self.board[dest] = set()

        self.where[piece] = dest
        self.board[OUT].remove(piece)
        self.board[dest].add(piece)

        return capture

    def _move(self, origin, dest):
        # a piece is moved from origin to dest
        OUT, WON = self.NODE_OUT, self.NODE_WON

        if origin in (OUT, WON):
            raise ValueError(f'attempting to move a piece from outside the board')
        if dest == OUT:  # piece can be captured but cannot willingly go out
            raise ValueError(f'attempting to move a piece out of board not allowed!')

        owner = self.cur_player  # +1 / -1
        pieces = self.board.pop(origin)  # remove moving pieces from board[origin]
        others = self.board.setdefault(dest, set())  # are there pieces at dest?
        capture = dest != WON and bool(len(others) and self.owner(next(iter(others))) != owner)

        if capture:
            for piece in others:
                self.where[piece] = OUT
            self.board[OUT].update(others)
            self.board[dest] = set()

        for piece in pieces:
            self.where[piece] = dest
        self.board[dest].update(pieces)

        return capture

    def _next_turn(self):
        assert max(self.history.keys()) == self.turn
        del self.history[self.turn]  # for now, no undo functionality
        self.turn += 1

    def config_render(self):
        if not hasattr(self, '_config_render'):
            from colorama import Fore, Style
            global colorama, Fore, Style

            def str_node(node, maxn=4, fill=' '):
                pieces = [Fore.CYAN + str(piece) + Fore.RESET if piece > 0 else \
                          Fore.RED + str(-piece) + Fore.RESET
                          for piece in self.board.get(node, set())] + [fill] * maxn
                return '{' + ''.join(pieces[:maxn]) + '}'

            def align(row, nodes):
                # nodes: stringified nodes list for this row (index)
                s = '  '
                if len(nodes) == 6:  # north / south (max #nodes)
                    return s.join(nodes)
                elif len(nodes) == 1:  # center (1 node)
                    return 10 * s + nodes[0] + 10 * s
                elif len(nodes) == 4:
                    if row in (1, 5):
                        return nodes[0] + s + nodes[1] + 9*s + nodes[2] + s + nodes[3]
                    else:
                        return nodes[0] + 5*s + nodes[1] + 1*s + nodes[2] + 5*s + nodes[3]
                else:
                    raise AssertionError(f'Align cannot handle row with {len(nodes)} #nodes')

            self._config_render = {
                'str_node': str_node,
                'align': align,
            }

        return self._config_render

    def render(self):
        config = self.config_render()

        str_node = config['str_node']
        align = config['align']

        N = self.N_PIECES_PER_PLAYER * len(self.players)
        pieces_won = str_node(self.NODE_WON, N)
        pieces_out = str_node(self.NODE_OUT, N)
        pieces_in = '\n'.join([
            align(row_idx, [str_node(node) for node in nodes_row])
            for row_idx, nodes_row in enumerate(self.graph.LAYOUT)
        ])
        
        print(Fore.RESET)
        print('WON =', pieces_won, end='\n')
        print('OUT =', pieces_out, end='\n\n')
        print(pieces_in)
        print(Style.RESET_ALL)


class Computer(object):

    def register_toss_outcome(self, toss_outcome):
        self.toss_outcome = toss_outcome

    def select_sticks(self, obs, is_com_turn):
        if is_com_turn:
            return random.choices([StickComb.UU, StickComb.MM, StickComb.MU],
                                  weights=[0.625, 0.225, 0.15], k=1)[0]
        else:
            return random.choices([StickComb.UU, StickComb.MM, StickComb.MU],
                                  weights=[0.175, 0.175, 0.65], k=1)[0]

    def select_orig_node(self, obs, valid_orig_nodes):
        return random.choice(list(valid_orig_nodes))

    def select_dest_node(self, obs, valid_dest_nodes):
        return random.choice(list(valid_dest_nodes))


class YutNoriHumVsCom(TacticalYutNori):

    valid_stick_pairs = (StickComb.MM, StickComb.MU, StickComb.UU)

    def reset(self, *args, **kwargs):
        com = kwargs.pop('computer', Computer())
        com_goes_first = kwargs.pop('computer_goes_first', False)
        obs = super().reset(*args, **kwargs)
        self.config(com, com_goes_first)
        return obs

    def config(self, computer, com_goes_first=False):
        self.computer = computer
        self.computer.player = self.players[0] if com_goes_first else self.players[1]

    def toss(self, hum_sticks):
        if hum_sticks not in self.valid_stick_pairs:
            raise ValueError(f'Invalid sticks {hum_sticks} selected!\n'
                             f'Valid stick pairs = {self.valid_stick_pairs}')
        is_com_turn = (self.cur_player == self.computer.player)
        com_sticks = self.computer.select_sticks(self.obs, is_com_turn)
        outcome = hum_sticks + com_sticks

        super().toss(outcome)

        if is_com_turn:
            self._computer_turn()

        # can player proceed and select?
        # True if is_hum_turn else False
        return (not is_com_turn)

    def is_computer_turn(self):
        return (self.cur_player == self.computer.player)

    def _computer_turn(self):
        self._during_computer_turn = True
        self.computer.register_toss_outcome(self.get_toss_outcome())

        valid_orig = self.valid_orig_nodes_for_selection()
        orig = self.computer.select_orig_node(self.obs, valid_orig)
        
        if orig in valid_orig:
            self.select_orig_node(orig)
            
            valid_dest = self.valid_dest_nodes_for_selection()
            dest = self.computer.select_dest_node(self.obs, valid_dest)
            
            if dest in valid_dest:
                self.select_dest_node(dest)

        del self._during_computer_turn

    def select_orig_node(self, node):
        if self.cur_player == self.computer.player and not hasattr(self, '_during_computer_turn'):
            raise ValueError('Human player cannot take this action during computer turn.')

        return super().select_orig_node(node)

    def select_dest_node(self, node):
        if self.cur_player == self.computer.player and not hasattr(self, '_during_computer_turn'):
            raise ValueError('Human player cannot take this action during computer turn.')

        return super().select_dest_node(node)






if __name__ == '__main__':

    game = YutNoriHumVsCom()
    n_games = 1
    debug = 1

    for _ in range(n_games):
        game.reset()

        while not game.check_winner():
            is_hum_turn = game.toss(
                random.choice([StickComb.MM, StickComb.UU, StickComb.MU])
            )

            if debug:
                ticker = '(HUM)' if is_hum_turn else '(COM)'
                print('Turn', game.turn)
                print('Player', game.cur_player, ticker)
                print('Toss', game.get_toss_outcome().name)

            if is_hum_turn:
                if not game.orig_node_been_selected():
                    game.select_orig_node(random.choice(list(
                        game.valid_orig_nodes_for_selection()))
                    )
                if not game.dest_node_been_selected():
                    game.select_dest_node(random.choice(list(
                        game.valid_dest_nodes_for_selection()))
                    )

            if debug:
                print('Orig', game.get_orig_node_selected())
                print('Dest', game.get_dest_node_selected())

            carried = game.step()
            if carried:
                if debug:
                    game.render()
            else:
                if debug:
                    print('Step FAIL')

        print('Winner', game.check_winner())
