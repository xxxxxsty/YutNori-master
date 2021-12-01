from tactical_yutnori import *
from itertools import islice
from math import hypot
import pygame
import pygame.gfxdraw
from pygame import Surface, Color, Rect
import ctypes



class App:

    config = dict(
        WSIZE   = (600, 600),
        FPS     = 30,
        TITLE   = 'Yut-Nori'
    )

    def __init__(self):
        self.game = YutNoriHumVsCom()
        self._config()

    def _config(self):
        self.WIDTH, self.HEIGHT = self.config['WSIZE']
        self.FPS = self.config['FPS']

        pygame.init()
        ctypes.windll.user32.SetProcessDPIAware()

        self.screen = pygame.display.set_mode((self.WIDTH, self.HEIGHT))
        self.clock = pygame.time.Clock()

        pygame.display.set_caption(self.config['TITLE'])

    def run(self):
        self.game.reset()
        self.GUI = GUI(self.game)

        stop = False

        while not stop:
            self.GUI.draw(self.screen)

            for event in pygame.event.get():
                if event.type == pygame.QUIT or \
                  (event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE):
                    stop = True

            self.GUI.update()

            pygame.display.flip()
            self.clock.tick(self.FPS)

        pygame.quit()


class Timer:

    def is_on(self, name=0):
        return hasattr(self, f't_{name}')

    def start(self, name=0):
        if not hasattr(self, f't_{name}'):
            setattr(self, f't_{name}', pygame.time.get_ticks())

    def elapsed(self, name=0):
        return (pygame.time.get_ticks() - getattr(self, f't_{name}')) / 1000.0

    def reset(self, name=0):
        delattr(self, f't_{name}')

    def restart(self):
        self.reset()
        self.start()


class GUI:

    def __init__(self, game):
        self.game = game

        # Constants and Refs
        self.PLAYER1, self.PLAYER2 = game.players
        self.N_PIECES_PER_PLAYER = game.N_PIECES_PER_PLAYER
        self.NODE_OUT, self.NODE_WON = game.NODE_OUT, game.NODE_WON

        self.isP1 = lambda p: self.game.owner(p) == self.PLAYER1
        self.isP2 = lambda p: self.game.owner(p) == self.PLAYER2

        # Position and Scaling
        W, H = pygame.display.get_surface().get_size()

        # Board Graph
        x, y, w, h = self.graph_rect = Rect([W//25, W//25, W*63//100, W*63//100])
        self.corner_node_radius = self.graph_rect.width//20
        self.normal_node_radius = self.graph_rect.width//30
        self.piece_radius = self.normal_node_radius*3//4

        # Regions out
        s = W - w   # space right
        self.regout1_rect = Rect([x + w + s//4, y, s//2, h*22//100])
        self.regout2_rect = Rect([x + w + s//4, y + h*33//100, s//2, h*22//100])

        # Table
        s = H - h  # space bottom
        *_, w, h = self.table_rect = Rect([x, H-s*3//4, w, s*3//4])
        self.sset_size = w * 15//100, h * 2//3
        self.stick_size = w * 6//100, h * 2//3

        # Nodes in (= Pieces in = centers rel to graph)
        self.nodes_rel_centers = self._arrange_nodes()
        self.nodes_abs_centers = self.get_nodes_abs_centers()

        # Pieces out (centers rel to regions out)
        self.piecesout_rel_centers = self._arrange_pieces()
        self.piecesout_abs_centers = self.get_piecesout_abs_centers()

        # Sticksets (toplefts rel to table)
        self.ssets_rel_toplefts = self._arrange_sticksets()
        self.ssets_abs_toplefts = self.get_sticksets_abs_toplefts()

        # Tossed-Sticks (toplefts rels to table)
        self.tossedsticks_rel_toplefts = self._arrange_tossedsticks()
        self.tossedsticks_abs_toplefts = self.get_tossedsticks_abs_toplefts()

        # Style
        self.BGCOLOR = Color(235,235,235)
        self.FONT = pygame.font.SysFont('Arial', 13)
        self.FONT.set_bold(True)

        # Nodes
        self.corner_style = (Color('orange'), Color('black'), 1)
        self.normal_style = (Color('white'), Color('black'), 1)
        self.hlight_style = (Color('lightblue'), 3)

        # Regions out (player1, player2)
        self.regout1_style = (Color('blue'), Color('black'), 2, 0.5)
        self.regout2_style = (Color('red'), Color('black'), 2, 0.5)

        # Pieces (player1, player2)
        self.player1_color_str, self.player2_color_str = 'BLUE', 'RED'
        self.piece1_style = (Color('blue'), Color('black'), 0)
        self.piece2_style = (Color('red'), Color('black'), 0)

        # Table and sticks
        self.table_style = (Color('brown'), Color('black'), 0, 0.95)
        self.stick_style = (Color('beige'), Color('black'), 2)

        # Static Images
        self.graph_im = self.get_graph_image()
        self.regout1_im, self.regout2_im = self.get_regionsout_images()
        self.table_im = self.get_table_image()
        self.stickM_im = self.get_stick_image(StickType.MARKED)
        self.stickU_im = self.get_stick_image(StickType.UNMARKED)
        self.ssets_im = self.get_sticksets_images()
        self.playerturn_im = self.get_playerturn_images()
        self.playerwins_im = self.get_playerwins_images()

        # Some Presentation Logic
        self.timer = Timer()
        self.timer.start()
        self.sset_sel = None
        self.node_sel = None
        self.hlight_nodes = []
        self.winner = None

    # UPDATE
    def update(self):

        if self.get_winner():
            self.check_replay_selection()
        
        else:
            
            if not self.timer.is_on('turn'):
                self.timer.start('turn')

            if not self.game.sticks_been_tossed():
                self.check_sticksets_selection()
                sel = self.get_stickset_selected()
                if sel in self.game.valid_stick_pairs:
                    self.game.toss(sel)
                    self.timer.start('delay')

             elif self.timer.elapsed('delay') > 0.5:
                
                if not self.game.orig_node_been_selected():
                        self.highlight_nodes(self.game.valid_orig_nodes_for_selection())
                        self.check_node_selection(self.hlight_nodes)
                        sel = self.get_node_selected()
                        if sel in self.game.valid_orig_nodes_for_selection():
                            self.game.select_orig_node(sel)

                elif not self.game.dest_node_been_selected():
                        self.highlight_nodes(self.game.valid_dest_nodes_for_selection())
                        self.check_node_selection(self.hlight_nodes)
                        sel = self.get_node_selected()
                        if sel in self.game.valid_dest_nodes_for_selection():
                            self.game.select_dest_node(sel)

                elif self.timer.elapsed('turn') > 1.8:
                    # everything selected -> proceed
                    ok = self.game.step()
                    if ok:
                        self.timer.reset('turn')
                        self.timer.reset('delay')
                        self.hlight_nodes = []
                        self.node_sel = None
                        self.sset_sel = None
                        self.check_winner()

    # DRAW
    def draw(self, surf):
        # static
        surf.fill(self.BGCOLOR)
        surf.blit(self.graph_im, self.graph_rect)
        surf.blit(self.regout1_im, self.regout1_rect)
        surf.blit(self.regout2_im, self.regout2_rect)
        surf.blit(self.table_im, self.table_rect)
        # dynamic
        self.draw_playerturn(surf)
        self.draw_highlighted_nodes(surf)
        self.draw_pieces(surf)
        self.draw_sticks(surf)
        self.draw_playerwins(surf)

    def draw_playerturn(self, surf):
        surf.blit(self.playerturn_im[self.game.cur_player], self.playerturn_rect)

    def draw_highlighted_nodes(self, surf):
        stroke, t = self.hlight_style

        for nodekey in self.hlight_nodes:

            if nodekey == self.NODE_OUT:
                radius = self.piece_radius
                pieces = (p for p in self.game.board[self.NODE_OUT] if self.game.owner(p) == self.game.cur_player)
                for piece, center in zip(pieces, self.piecesout_abs_centers[self.game.cur_player]):
                    pygame.draw.circle(surf, stroke, center, radius + t, t)

            elif nodekey == self.NODE_WON:
                radius = self.corner_node_radius
                x, y = self.nodes_abs_centers[BoardNode.CORNERS.SE]
                x += 4 * radius
                pygame.gfxdraw.aacircle(surf, x, y, radius+t, Color('gold'))
                pygame.gfxdraw.filled_circle(surf, x, y, radius+t, Color('gold'))
                fill, stroke, t = self.normal_style
                pygame.gfxdraw.aacircle(surf, x, y, radius, stroke)
                pygame.gfxdraw.filled_circle(surf, x, y, radius, stroke)
                pygame.gfxdraw.aacircle(surf, x, y, radius-t-t, fill)
                pygame.gfxdraw.filled_circle(surf, x, y, radius-t-t, fill)

            else:
                radius = self.corner_node_radius if nodekey in BoardNode.CORNERS else \
                         self.normal_node_radius
                center = self.nodes_abs_centers[nodekey]
                pygame.draw.circle(surf, stroke, center, radius + t, t)

    def draw_pieces(self, surf):
        piecesout1 = (p for p in self.piecesout_abs_centers[self.PLAYER1])
        piecesout2 = (p for p in self.piecesout_abs_centers[self.PLAYER2])

        for piece, nodekey in self.game.all_pieces_and_nodes():

            if nodekey == self.NODE_OUT:
                if self.isP1(piece):
                    x, y = next(piecesout1)
                    fill, stroke, t = self.piece1_style
                else:
                    x, y = next(piecesout2)
                    fill, stroke, t = self.piece2_style

                pygame.gfxdraw.aacircle(surf, x, y, self.piece_radius, stroke)
                pygame.gfxdraw.filled_circle(surf, x, y, self.piece_radius, stroke)
                pygame.gfxdraw.aacircle(surf, x, y, self.piece_radius-t-t, fill)
                pygame.gfxdraw.filled_circle(surf, x, y, self.piece_radius-t-t, fill)

            elif nodekey != self.NODE_WON:
                x, y = self.nodes_abs_centers[nodekey]
                fill, stroke, t = self.piece1_style if self.isP1(piece) else \
                                  self.piece2_style

                pygame.gfxdraw.aacircle(surf, x, y, self.piece_radius, stroke)
                pygame.gfxdraw.filled_circle(surf, x, y, self.piece_radius, stroke)
                pygame.gfxdraw.aacircle(surf, x, y, self.piece_radius-t-t, fill)
                pygame.gfxdraw.filled_circle(surf, x, y, self.piece_radius-t-t, fill)

                # pieces are stacked
                if len(self.game.board[nodekey]) > 1:
                    n_stacked = len(self.game.board[nodekey])
                    text_surf = self.FONT.render(str(n_stacked), True, Color('white'))
                    text_rect = text_surf.get_rect(center=(x, y))
                    surf.blit(text_surf, text_rect)

    def draw_sticks(self, surf):
        if self.game.sticks_been_tossed():
            outcome = self.game.get_toss_outcome()
            for stick_type, xy in zip(outcome, self.tossedsticks_abs_toplefts):
                stick_im = self.stickM_im if stick_type == StickType.M else self.stickU_im
                surf.blit(stick_im, xy)
        else:
            for sset_im, xy in zip(self.ssets_im.values(), self.ssets_abs_toplefts):
                surf.blit(sset_im, xy)

    def draw_playerwins(self, surf):
        winner = self.get_winner()
        if winner:
            winner_im = self.playerwins_im[winner]
            surf.blit(winner_im, winner_im.get_rect(center=self.playerwins_rect.center))

    # STATIC IMAGES
    def get_graph_image(self):
        surf = Surface(self.graph_rect.size)
        surf.fill(self.BGCOLOR)
        surf.set_colorkey(self.BGCOLOR)

        for name, center in self.nodes_rel_centers.items():
            if name in BoardNode.CORNERS:
                fill, stroke, t = self.corner_style
                radius = self.corner_node_radius
            else:
                fill, stroke, t = self.normal_style
                radius = self.normal_node_radius
            pygame.gfxdraw.aacircle(surf, *center, radius, stroke)
            pygame.gfxdraw.filled_circle(surf, *center, radius, stroke)
            pygame.gfxdraw.aacircle(surf, *center, radius-t-t, fill)
            pygame.gfxdraw.filled_circle(surf, *center, radius-t-t, fill)

        return surf.convert_alpha()

    def get_regionsout_images(self):
        surf1 = Surface(self.regout1_rect.size)
        surf2 = Surface(self.regout2_rect.size)

        fill1, stroke1, t1, alpha1 = self.regout1_style
        fill2, stroke2, t2, alpha2 = self.regout2_style

        surf1.fill(stroke1)
        surf1.fill(fill1, (t1, t1, surf1.get_width()-t1-t1, surf1.get_height()-t1-t1))

        surf2.fill(stroke2)
        surf2.fill(fill2, (t2, t2, surf2.get_width()-t2-t2, surf2.get_height()-t2-t2))

        surf1.set_alpha(int(alpha1 * 255))
        surf2.set_alpha(int(alpha2 * 255))

        return surf1.convert_alpha(), surf2.convert_alpha()

    def get_table_image(self):
        surf = Surface(self.table_rect.size)

        fill, stroke, t, alpha = self.table_style

        surf.fill(stroke)
        surf.fill(fill, (t, t, surf.get_width()-t-t, surf.get_height()-t-t))
        surf.set_alpha(int(alpha * 255))

        return surf.convert_alpha()

    def get_sticksets_images(self):
        surf_mm = Surface(self.sset_size)
        surf_mu = Surface(self.sset_size)
        surf_uu = Surface(self.sset_size)

        for surf in (surf_mm, surf_mu, surf_uu):
            surf.fill(self.table_style[0])
            surf.set_colorkey(surf.get_at([0, 0]))

        sset_w, sset_h = self.sset_size
        stick_w, stick_h = self.stick_size
        sx = sset_w - 2 * stick_w
        sy = sset_h - stick_h

        surf_mm.blit(self.stickM_im, (0, sy))
        surf_mm.blit(self.stickM_im, (stick_w + sx, sy))
        surf_mu.blit(self.stickM_im, (0, sy))
        surf_mu.blit(self.stickU_im, (stick_w + sx, sy))
        surf_uu.blit(self.stickU_im, (0, sy))
        surf_uu.blit(self.stickU_im, (stick_w + sx, sy))

        return {StickComb.MM: surf_mm.convert_alpha(),
                StickComb.MU: surf_mu.convert_alpha(),
                StickComb.UU: surf_uu.convert_alpha()}

    def get_stick_image(self, stick_type):
        assert stick_type in StickType, \
             f'Invalid stick type {stick_type!r} given!\n' \
             f'Valid stick types = {StickType!r}.'

        surf = Surface(self.stick_size)

        fill, stroke, t = self.stick_style

        surf.fill(stroke)
        surf.fill(fill, (t, t, surf.get_width()-t-t, surf.get_height()-t-t))

        if stick_type == StickType.MARKED:
            # draw three vertical crosses on top
            n = 3
            r = surf.get_width() // 5
            cross_h = r + r

            # spacing
            sx = surf.get_width() // 2
            sy = (surf.get_height() - n * cross_h) // (n + 1)

            # offset
            x = sx
            y = sy + r

            for i in range(n):
                cx, cy = (x, y + (sy + cross_h) * i)
                pygame.draw.line(surf, stroke, (cx-r, cy-r), (cx+r, cy+r), t)
                pygame.draw.line(surf, stroke, (cx-r, cy+r), (cx+r, cy-r), t)

        return surf

    def get_playerturn_images(self):
        # settings
        player1_color_str = self.player1_color_str
        player2_color_str = self.player2_color_str
        player1_color = self.piece1_style[0]
        player2_color = self.piece2_style[0]

        # render font
        font = pygame.font.SysFont('Arial', 13)
        surf1 = font.render(f'{player1_color_str} TURN', True, player1_color)
        surf2 = font.render(f'{player2_color_str} TURN', True, player2_color)

        # position between the out-regions
        x0, y0 = self.regout1_rect.bottomleft
        x1, y1 = self.regout2_rect.topright
        x = x0 + (x1 - x0) // 2
        y = y0 + (y1 - y0) // 2
        self.playerturn_rect = surf1.get_rect(center=(x, y))
        
        return {self.PLAYER1: surf1,
                self.PLAYER2: surf2}

    def get_playerwins_images(self):
        # settings
        player1_color_str = self.player1_color_str
        player2_color_str = self.player2_color_str
        player1_color = self.piece1_style[0]
        player2_color = self.piece2_style[0]

        # render font
        font = pygame.font.SysFont('Arial', 56)
        surf1 = font.render(f'{player1_color_str} WINS', True, Color('gold'))
        surf2 = font.render(f'{player2_color_str} WINS', True, Color('gold'))

        # position center
        self.playerwins_rect = surf1.get_rect()
        self.playerwins_rect.center = self.nodes_abs_centers[BoardNode.CORNERS.CC]

        bg = Surface(self.playerwins_rect.size)

        bg1 = bg.copy()
        bg1.fill(player1_color)
        bg1.blit(surf1, (0, 0))

        bg2 = bg.copy()
        bg2.fill(player2_color)
        bg2.blit(surf2, (0, 0))

        return {self.PLAYER1: bg1.convert_alpha(),
                self.PLAYER2: bg2.convert_alpha()}

    # LAYOUT / POSITIONING
    def vert(self, cornerA, cornerB, n_nodes=4):
        (x, Ay), (x, By) = cornerA, cornerB

        R = self.corner_node_radius
        dy = (abs(By - Ay) - 2*R) // (n_nodes + 1)
        dy = +dy if Ay < By else -dy

        beg_y = Ay + R if Ay < By else Ay - R

        return [(x, beg_y + i * dy) for i in range(1, n_nodes+1)]

    def horz(self, cornerA, cornerB, n_nodes=4):
        (Ax, y), (Bx, y) = cornerA, cornerB

        R = self.corner_node_radius
        dx = (abs(Bx - Ax) - 2*R) // (n_nodes + 1)
        dx = +dx if Ax < Bx else -dx

        beg_x = Ax + R if Ax < Bx else Ax - R

        return [(beg_x + i * dx, y) for i in range(1, n_nodes+1)]

    def diag(self, cornerA, cornerB, n_nodes=2):
        (Ax, Ay), (Bx, By) = cornerA, cornerB

        R = self.corner_node_radius
        d = (abs(Bx - Ax) - 2*R) // (n_nodes + 1)
        dx = +d if Ax < Bx else -d
        dy = +d if Ay < By else -d

        beg_x = Ax + R if Ax < Bx else Ax - R
        beg_y = Ay + R if Ay < By else Ay - R

        return [(beg_x + i * dx, beg_y + i * dy) for i in range(1, n_nodes+1)]

    def _arrange_nodes(self):
        nodes = {}

        # corners positions (including center)
        S = self.graph_rect.width
        R = self.corner_node_radius

        nodes[BoardNode.CORNERS.SW] = sw = (R, S-1-R)
        nodes[BoardNode.CORNERS.NW] = nw = (R, R)
        nodes[BoardNode.CORNERS.CC] = cc = (S//2, S//2)
        nodes[BoardNode.CORNERS.NE] = ne = (S-1-R, R)
        nodes[BoardNode.CORNERS.SE] = se = (S-1-R, S-1-R)

        nodes[BoardNode.SPECIAL.WON] = (S+R+R, S-1-R)

        # circuit nodes (clockwise) for vertical and horizontal paths
        n_nodes = 4
        # se -> ne (vertical)
        for nodekey, xy in zip(BoardNode.EAST, self.vert(se, ne, n_nodes)):
            nodes[nodekey] = xy
        # ne -> nw (horizontal)
        for nodekey, xy in zip(BoardNode.NORTH, self.horz(ne, nw, n_nodes)):
            nodes[nodekey] = xy
        # nw -> sw (vertical)
        for nodekey, xy in zip(BoardNode.WEST, self.vert(nw, sw, n_nodes)):
            nodes[nodekey] = xy
        # sw -> se (horizontal)
        for nodekey, xy in zip(BoardNode.SOUTH, self.horz(sw, se, n_nodes)):
            nodes[nodekey] = xy

        # circuit nodes for diagonal paths
        n_nodes = 2
        NORTH_EAST_UPPER = islice(BoardNode.NORTH_EAST, 0, 2)
        NORTH_EAST_LOWER = islice(BoardNode.NORTH_EAST, 2, 4)
        NORTH_WEST_UPPER = islice(BoardNode.NORTH_WEST, 0, 2)
        NORTH_WEST_LOWER = islice(BoardNode.NORTH_WEST, 2, 4)
        # ne -> cc
        for nodekey, xy in zip(NORTH_EAST_UPPER, self.diag(ne, cc, n_nodes)):
            nodes[nodekey] = xy
        # cc -> sw
        for nodekey, xy in zip(NORTH_EAST_LOWER, self.diag(cc, sw, n_nodes)):
            nodes[nodekey] = xy
        # nw -> cc
        for nodekey, xy in zip(NORTH_WEST_UPPER, self.diag(nw, cc, n_nodes)):
            nodes[nodekey] = xy
        # cc -> se
        for nodekey, xy in zip(NORTH_WEST_LOWER, self.diag(cc, se, n_nodes)):
            nodes[nodekey] = xy

        return nodes

    def _arrange_pieces(self):
        n = self.N_PIECES_PER_PLAYER
        r = self.piece_radius
        diam = r + r

        # regions
        x1, y1, (w1, h1) = 0, 0, self.regout1_rect.size
        x2, y2, (w2, h2) = 0, 0, self.regout2_rect.size

        # spacing
        sx1 = (w1 - diam * n) // (n + 1)
        sy1 = (h1 - diam * 1) // 2
        sx2 = (w2 - diam * n) // (n + 1)
        sy2 = (h2 - diam * 1) // 2

        # offset
        x1 += sx1 + r
        y1 += sy1 + r
        x2 += sx2 + r
        y2 += sy2 + r

        pieces1 = [(x1 + (sx1 + diam) * i, y1) for i in range(n)]
        pieces2 = [(x2 + (sx2 + diam) * i, y2) for i in range(n)]

        return {self.PLAYER1: pieces1, self.PLAYER2: pieces2}

    def _arrange_sticksets(self):
        table_w, table_h = self.table_rect.size
        w, h = self.sset_size
        n = 3  # num sticksets (marked (mm), mismatch (mu), unmarked (uu))

        # spacing
        sx = (table_w - w * n) // (n + 1)
        sy = (table_h - h * 1) // 2

        # rel offset (from table)
        x = sx
        y = sy

        # toplefts
        xys = [(x + (sx + w) * i, y) for i in range(n)]

        return xys

    def _arrange_tossedsticks(self):
        table_w, table_h = self.table_rect.size
        stick_w, stick_h = self.stick_size
        n = 4  # num tossed sticks

        # spacing
        sx = stick_w

        # all four tossedsticks size
        w, h = (stick_w + sx) * n, stick_h
        self.tossedsticks_size = (w, h)

        # offset (centering)
        x = (table_w - w) // 2
        y = (table_h - h) // 2

        # toplefts
        sticks_xys = [(x + (stick_w + sx) * i, y) for i in range(n)]

        return sticks_xys

    def get_nodes_abs_centers(self):
        # = pieces in abs centers (same centers with smaller radius)
        x, y = self.graph_rect.topleft
        return {nodekey: (x + dx, y + dy) for nodekey, (dx, dy) in self.nodes_rel_centers.items()}

    def get_piecesout_abs_centers(self):
        x1, y1 = self.regout1_rect.topleft
        x2, y2 = self.regout2_rect.topleft
        P1, P2 = self.PLAYER1, self.PLAYER2
        return {P1: [(x1 + dx1, y1 + dy1) for dx1, dy1 in self.piecesout_rel_centers[P1]],
                P2: [(x2 + dx2, y2 + dy2) for dx2, dy2 in self.piecesout_rel_centers[P2]]}

    def get_sticksets_abs_toplefts(self):
        x, y = self.table_rect.topleft
        return [(x + dx, y + dy) for dx, dy in self.ssets_rel_toplefts]

    def get_tossedsticks_abs_toplefts(self):
        x, y = self.table_rect.topleft
        return [(x + dx, y + dy) for dx, dy in self.tossedsticks_rel_toplefts]

    # INTERACTIONS
    def get_node_selected(self):
        return self.node_sel

    def get_stickset_selected(self):
        return self.sset_sel

    def get_winner(self):
        return self.winner

    def highlight_nodes(self, nodes):
        self.hlight_nodes = nodes

    def check_node_selection(self, nodes):
        pressed = pygame.mouse.get_pressed()[0]

        if pressed:
            ux, uy = pygame.mouse.get_pos()
            for nodekey in nodes:
                if nodekey == self.NODE_OUT:
                    r = self.piece_radius
                    for x, y in self.piecesout_abs_centers[self.game.cur_player]:
                        if hypot(y-uy, x-ux) < r:
                            self.node_sel = nodekey
                            return nodekey
                else:
                    x, y = self.nodes_abs_centers[nodekey]
                    r = self.normal_node_radius
                    if nodekey in BoardNode.CORNERS or nodekey == self.NODE_WON:
                        r = self.corner_node_radius
                    if hypot(y-uy, x-ux) < r:
                        self.node_sel = nodekey
                        return nodekey

        self.node_sel = None
        return None

    def check_sticksets_selection(self):
        pressed = pygame.mouse.get_pressed()[0]

        if pressed:
            ux, uy = pygame.mouse.get_pos()
            for (comb, sset_im), (x, y) in zip(self.ssets_im.items(), self.ssets_abs_toplefts):
                rect = Rect(x, y, *sset_im.get_size())
                if rect.collidepoint(ux, uy):
                    self.sset_sel = comb
                    return comb

        self.sset_sel = None
        return None

    def check_replay_selection(self):
        pressed = pygame.mouse.get_pressed()[0]

        if pressed:
            ux, uy = pygame.mouse.get_pos()
            if self.playerwins_rect.collidepoint(ux, uy):
                self.game.reset()
                self.check_winner()

    def check_winner(self):
        self.winner = self.game.check_winner()



if __name__ == '__main__':
    App().run()
