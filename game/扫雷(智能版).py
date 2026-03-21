import tkinter as tk
from tkinter import messagebox
import random
import time
import json
import os
import sys

class Minesweeper:
    def __init__(self, master):
        self.master = master
        master.title("扫雷游戏")
        # 读取配置文件
        self.load_config()
        self.create_selector()
        self.create_game_board(9, 9, 10)

    def load_config(self):
        """读取配置文件，如果外部不存在则尝试从打包内部复制一份默认配置"""
        default_config = {
            "enable_auto_sweep": True,
            "auto_restart_on_loss": True
        }
        # 可执行文件所在目录
        exe_dir = os.path.dirname(sys.executable) if getattr(sys, 'frozen', False) else os.path.dirname(__file__)
        config_path = os.path.join(exe_dir, "config.json")

        # 1. 尝试读取外部配置文件
        if os.path.exists(config_path):
            try:
                with open(config_path, 'r', encoding='utf-8') as f:
                    config = json.load(f)
                self.enable_auto_sweep = config.get("enable_auto_sweep", default_config["enable_auto_sweep"])
                self.auto_restart_on_loss = config.get("auto_restart_on_loss", default_config["auto_restart_on_loss"])
                return
            except Exception:
                pass

        # 2. 外部配置文件不存在或读取失败，尝试从打包内部读取默认配置
        if getattr(sys, 'frozen', False):
            # 打包运行状态，内部临时目录由 sys._MEIPASS 提供
            internal_config_path = os.path.join(sys._MEIPASS, "config.json")
            if os.path.exists(internal_config_path):
                try:
                    with open(internal_config_path, 'r', encoding='utf-8') as f:
                        config = json.load(f)
                    self.enable_auto_sweep = config.get("enable_auto_sweep", default_config["enable_auto_sweep"])
                    self.auto_restart_on_loss = config.get("auto_restart_on_loss", default_config["auto_restart_on_loss"])
                except Exception:
                    self.enable_auto_sweep = default_config["enable_auto_sweep"]
                    self.auto_restart_on_loss = default_config["auto_restart_on_loss"]
            else:
                self.enable_auto_sweep = default_config["enable_auto_sweep"]
                self.auto_restart_on_loss = default_config["auto_restart_on_loss"]
        else:
            # 源码运行模式
            self.enable_auto_sweep = default_config["enable_auto_sweep"]
            self.auto_restart_on_loss = default_config["auto_restart_on_loss"]

        # 3. 在外部目录生成一份默认配置文件，方便用户修改
        try:
            with open(config_path, 'w', encoding='utf-8') as f:
                json.dump(default_config, f, indent=4, ensure_ascii=False)
        except Exception:
            pass

    def create_selector(self):
        select_frame = tk.Frame(self.master)
        select_frame.pack(fill=tk.X, padx=5, pady=5)

        tk.Label(select_frame, text="难度：").pack(side=tk.LEFT)

        self.difficulty_var = tk.StringVar(value="初级")
        difficulty_menu = tk.OptionMenu(select_frame, self.difficulty_var,
                                        "初级", "中级", "高级",
                                        command=self.change_difficulty)
        difficulty_menu.pack(side=tk.LEFT, padx=5)

        self.mines_label = tk.Label(select_frame, text="剩余雷数: 10", font=("Arial", 12))
        self.mines_label.pack(side=tk.LEFT, padx=20)

        self.status_label = tk.Label(select_frame, text="游戏进行中", font=("Arial", 12))
        self.status_label.pack(side=tk.LEFT, padx=10)

        # 计时器标签
        self.timer_label = tk.Label(select_frame, text="时间: 0s", font=("Arial", 12))
        self.timer_label.pack(side=tk.LEFT, padx=10)

        reset_btn = tk.Button(select_frame, text="新游戏", command=self.reset_game)
        reset_btn.pack(side=tk.LEFT, padx=10)

        # 根据配置决定是否显示自动扫雷按钮
        if self.enable_auto_sweep:
            self.auto_btn = tk.Button(select_frame, text="自动扫雷", command=self.auto_sweep)
            self.auto_btn.pack(side=tk.LEFT, padx=10)
        else:
            self.auto_btn = None

    def create_game_board(self, rows, cols, mines):
        if hasattr(self, 'canvas'):
            self.canvas.destroy()
        self.rows = rows
        self.cols = cols
        self.total_mines = mines
        self.remaining_mines = mines
        cell_size = 30
        self.canvas = tk.Canvas(
            self.master,
            width=cols * cell_size,
            height=rows * cell_size,
            bg="gray"
        )
        self.canvas.pack(padx=5, pady=5)
        self.canvas.bind("<Button-1>", self.left_click)
        self.canvas.bind("<Button-3>", self.right_click)
        self.mines_map = [[False] * cols for _ in range(rows)]
        self.numbers = [[0] * cols for _ in range(rows)]
        self.revealed = [[False] * cols for _ in range(rows)]
        self.flagged = [[False] * cols for _ in range(rows)]
        self.game_over_flag = False
        self.win_flag = False
        self.rect_ids = [[None] * cols for _ in range(rows)]
        self.text_ids = [[None] * cols for _ in range(rows)]
        self.draw_grid()
        self.setup_game()

    def draw_grid(self):
        cell_size = 30
        for r in range(self.rows):
            for c in range(self.cols):
                x1 = c * cell_size
                y1 = r * cell_size
                x2 = x1 + cell_size
                y2 = y1 + cell_size
                rect_id = self.canvas.create_rectangle(x1, y1, x2, y2, fill="lightgray", outline="gray")
                text_id = self.canvas.create_text(x1 + cell_size//2, y1 + cell_size//2, text="", font=("Arial", 12, "bold"))
                self.rect_ids[r][c] = rect_id
                self.text_ids[r][c] = text_id

    def setup_game(self):
        for r in range(self.rows):
            for c in range(self.cols):
                self.mines_map[r][c] = False
                self.numbers[r][c] = 0
                self.revealed[r][c] = False
                self.flagged[r][c] = False
                self.canvas.itemconfig(self.rect_ids[r][c], fill="lightgray")
                self.canvas.itemconfig(self.text_ids[r][c], text="")
        self.game_over_flag = False
        self.win_flag = False
        self.remaining_mines = self.total_mines
        self.update_mines_display()
        self.status_label.config(text="游戏进行中")
        # 重置计时器
        self.start_time = None
        self.timer_running = False
        self.update_timer_display(0)

        # 随机布雷
        mines_placed = 0
        while mines_placed < self.total_mines:
            r = random.randint(0, self.rows - 1)
            c = random.randint(0, self.cols - 1)
            if not self.mines_map[r][c]:
                self.mines_map[r][c] = True
                mines_placed += 1

        # 计算数字
        for r in range(self.rows):
            for c in range(self.cols):
                if self.mines_map[r][c]:
                    self.numbers[r][c] = -1
                else:
                    count = 0
                    for dr in (-1, 0, 1):
                        for dc in (-1, 0, 1):
                            nr, nc = r + dr, c + dc
                            if 0 <= nr < self.rows and 0 <= nc < self.cols and self.mines_map[nr][nc]:
                                count += 1
                    self.numbers[r][c] = count

    def start_timer(self):
        """开始计时"""
        if not self.timer_running and not self.game_over_flag and not self.win_flag:
            self.start_time = time.time()
            self.timer_running = True
            self.update_timer()

    def update_timer(self):
        """定时更新时间显示"""
        if self.timer_running and not self.game_over_flag and not self.win_flag:
            elapsed = int(time.time() - self.start_time)
            self.update_timer_display(elapsed)
            self.master.after(1000, self.update_timer)

    def update_timer_display(self, seconds):
        """更新计时器标签"""
        if seconds < 60:
            self.timer_label.config(text=f"时间: {seconds}s")
        else:
            minutes = seconds // 60
            secs = seconds % 60
            self.timer_label.config(text=f"时间: {minutes:02d}:{secs:02d}")

    def stop_timer(self):
        """停止计时"""
        self.timer_running = False

    def change_difficulty(self, choice):
        if choice == "初级":
            self.create_game_board(9, 9, 10)
        elif choice == "中级":
            self.create_game_board(16, 16, 40)
        elif choice == "高级":
            self.create_game_board(20, 20, 80)

    def reset_game(self):
        self.setup_game()

    def left_click(self, event):
        if self.game_over_flag or self.win_flag:
            return
        cell_size = 30
        x, y = event.x, event.y
        col = x // cell_size
        row = y // cell_size
        if 0 <= row < self.rows and 0 <= col < self.cols:
            if self.flagged[row][col]:
                return
            if self.revealed[row][col]:
                return
            # 首次点击时开始计时
            if self.start_time is None:
                self.start_timer()
            self.reveal(row, col)

    def right_click(self, event):
        if self.game_over_flag or self.win_flag:
            return
        cell_size = 30
        x, y = event.x, event.y
        col = x // cell_size
        row = y // cell_size
        if 0 <= row < self.rows and 0 <= col < self.cols:
            if self.revealed[row][col]:
                return
            if not self.flagged[row][col]:
                self.flagged[row][col] = True
                self.canvas.itemconfig(self.text_ids[row][col], text="🚩", fill="red")
                self.remaining_mines -= 1
                self.update_mines_display()
            else:
                self.flagged[row][col] = False
                self.canvas.itemconfig(self.text_ids[row][col], text="")
                self.remaining_mines += 1
                self.update_mines_display()

    def reveal(self, row, col):
        if self.revealed[row][col] or self.flagged[row][col]:
            return
        self.revealed[row][col] = True
        num = self.numbers[row][col]
        if num == -1:
            self.game_over()
            return
        self.update_cell_display(row, col)
        if num == 0:
            self.reveal_zeros(row, col)
        self.check_win()

    def reveal_zeros(self, row, col):
        stack = [(row, col)]
        visited = set()
        visited.add((row, col))
        while stack:
            r, c = stack.pop()
            for dr in (-1, 0, 1):
                for dc in (-1, 0, 1):
                    nr, nc = r + dr, c + dc
                    if 0 <= nr < self.rows and 0 <= nc < self.cols:
                        if not self.revealed[nr][nc] and not self.flagged[nr][nc] and (nr, nc) not in visited:
                            visited.add((nr, nc))
                            if self.numbers[nr][nc] == 0:
                                self.revealed[nr][nc] = True
                                self.update_cell_display(nr, nc)
                                stack.append((nr, nc))
                            elif self.numbers[nr][nc] > 0:
                                self.revealed[nr][nc] = True
                                self.update_cell_display(nr, nc)

    def update_cell_display(self, row, col):
        num = self.numbers[row][col]
        if num == -1:
            self.canvas.itemconfig(self.rect_ids[row][col], fill="red")
            self.canvas.itemconfig(self.text_ids[row][col], text="💣")
        else:
            color = self.get_number_color(num)
            self.canvas.itemconfig(self.rect_ids[row][col], fill="white")
            if num > 0:
                self.canvas.itemconfig(self.text_ids[row][col], text=str(num), fill=color)
            else:
                self.canvas.itemconfig(self.text_ids[row][col], text="")

    def get_number_color(self, num):
        colors = {1: "blue", 2: "green", 3: "red", 4: "purple", 5: "maroon", 6: "turquoise", 7: "black", 8: "gray"}
        return colors.get(num, "black")

    def check_win(self):
        for r in range(self.rows):
            for c in range(self.cols):
                if not self.mines_map[r][c] and not self.revealed[r][c]:
                    return False
        self.win_flag = True
        self.status_label.config(text="游戏胜利！")
        # 停止计时并获取最终时间
        self.stop_timer()
        elapsed = int(time.time() - self.start_time) if self.start_time else 0
        self.update_timer_display(elapsed)
        messagebox.showinfo("胜利", f"恭喜你，成功排除了所有地雷！\n用时: {self.format_time(elapsed)}")
        return True

    def format_time(self, seconds):
        """格式化时间显示"""
        if seconds < 60:
            return f"{seconds}秒"
        else:
            minutes = seconds // 60
            secs = seconds % 60
            return f"{minutes}分{secs}秒"

    def game_over(self):
        self.game_over_flag = True
        self.status_label.config(text="游戏失败")
        self.stop_timer()  # 停止计时
        for r in range(self.rows):
            for c in range(self.cols):
                if self.mines_map[r][c]:
                    self.canvas.itemconfig(self.rect_ids[r][c], fill="red")
                    self.canvas.itemconfig(self.text_ids[r][c], text="💣")
        messagebox.showinfo("游戏结束", "你踩到地雷了！")
        if self.auto_restart_on_loss:
            self.master.after(100, self.reset_game)

    def update_mines_display(self):
        self.mines_label.config(text=f"剩余雷数: {self.remaining_mines}")

    def ai_right_click(self, row, col):
        if self.game_over_flag or self.win_flag:
            return
        if self.revealed[row][col]:
            return
        if not self.flagged[row][col]:
            self.flagged[row][col] = True
            self.canvas.itemconfig(self.text_ids[row][col], text="🚩", fill="red")
            self.remaining_mines -= 1
            self.update_mines_display()
        else:
            self.flagged[row][col] = False
            self.canvas.itemconfig(self.text_ids[row][col], text="")
            self.remaining_mines += 1
            self.update_mines_display()

    def auto_sweep(self):
        if self.game_over_flag or self.win_flag:
            return
        if not self.enable_auto_sweep:
            return
        if self.auto_btn:
            self.auto_btn.config(state=tk.DISABLED)
        self.master.update()

        max_attempts = 10
        for attempt in range(max_attempts):
            if self.win_flag:
                break
            if self.game_over_flag:
                self.reset_game()
                self.master.update()
                time.sleep(0.5)

            ai = AutoSweeper(self)
            success = ai.solve()
            if success:
                break

        if self.auto_btn:
            self.auto_btn.config(state=tk.NORMAL)


class AutoSweeper:
    def __init__(self, game):
        self.game = game

    def get_neighbors(self, r, c):
        neighbors = []
        for dr in (-1, 0, 1):
            for dc in (-1, 0, 1):
                if dr == 0 and dc == 0:
                    continue
                nr, nc = r + dr, c + dc
                if 0 <= nr < self.game.rows and 0 <= nc < self.game.cols:
                    neighbors.append((nr, nc))
        return neighbors

    def solve(self):
        max_steps = 10000
        for _ in range(max_steps):
            if self.game.win_flag:
                return True
            if self.game.game_over_flag:
                return False

            changed = self.infer()
            if not changed:
                guess = self.best_guess()
                if guess:
                    # 确保在第一次点击时开始计时
                    if self.game.start_time is None:
                        self.game.start_timer()
                    self.game.reveal(guess[0], guess[1])
                else:
                    break
            self.game.master.update()
            time.sleep(0.01)
        return self.game.win_flag

    def infer(self):
        moved = False

        # 基础规则
        for r in range(self.game.rows):
            for c in range(self.game.cols):
                if not self.game.revealed[r][c]:
                    continue
                num = self.game.numbers[r][c]
                if num <= 0:
                    continue
                neighbors = self.get_neighbors(r, c)
                unknown = []
                flagged = 0
                for nr, nc in neighbors:
                    if self.game.flagged[nr][nc]:
                        flagged += 1
                    elif not self.game.revealed[nr][nc]:
                        unknown.append((nr, nc))
                if flagged == num and unknown:
                    for nr, nc in unknown:
                        self.game.reveal(nr, nc)
                        moved = True
                elif len(unknown) == num - flagged and unknown:
                    for nr, nc in unknown:
                        self.game.ai_right_click(nr, nc)
                        moved = True

        # 约束求解
        moved = moved or self.constraint_solve()
        return moved

    def constraint_solve(self):
        unknown_cells = set()
        for r in range(self.game.rows):
            for c in range(self.game.cols):
                if not self.game.revealed[r][c] and not self.game.flagged[r][c]:
                    unknown_cells.add((r, c))
        if not unknown_cells:
            return False

        constraints = []
        for r in range(self.game.rows):
            for c in range(self.game.cols):
                if not self.game.revealed[r][c]:
                    continue
                num = self.game.numbers[r][c]
                if num <= 0:
                    continue
                neighbors = self.get_neighbors(r, c)
                unknown = []
                flagged = 0
                for nr, nc in neighbors:
                    if self.game.flagged[nr][nc]:
                        flagged += 1
                    elif not self.game.revealed[nr][nc]:
                        unknown.append((nr, nc))
                if unknown:
                    needed = num - flagged
                    constraints.append((set(unknown), needed))

        constraints.sort(key=lambda x: len(x[0]))
        changed = False
        for i in range(len(constraints)):
            set_i, need_i = constraints[i]
            for j in range(len(constraints)):
                if i == j:
                    continue
                set_j, need_j = constraints[j]
                if set_i.issubset(set_j):
                    diff = set_j - set_i
                    if diff and need_j - need_i >= 0:
                        if need_j - need_i == len(diff):
                            for cell in diff:
                                if not self.game.flagged[cell[0]][cell[1]]:
                                    self.game.ai_right_click(cell[0], cell[1])
                                    changed = True
                        elif need_j - need_i == 0:
                            for cell in diff:
                                if not self.game.revealed[cell[0]][cell[1]]:
                                    self.game.reveal(cell[0], cell[1])
                                    changed = True
        return changed

    def best_guess(self):
        unknown_cells = []
        for r in range(self.game.rows):
            for c in range(self.game.cols):
                if not self.game.revealed[r][c] and not self.game.flagged[r][c]:
                    unknown_cells.append((r, c))

        if not unknown_cells:
            return None

        revealed_exist = any(self.game.revealed[r][c] for r in range(self.game.rows) for c in range(self.game.cols))
        if not revealed_exist:
            corners = [(0, 0), (0, self.game.cols-1), (self.game.rows-1, 0), (self.game.rows-1, self.game.cols-1)]
            valid_corners = [c for c in corners if c in unknown_cells]
            if valid_corners:
                return random.choice(valid_corners)
            else:
                return random.choice(unknown_cells)

        prob = {cell: 0.0 for cell in unknown_cells}
        for r in range(self.game.rows):
            for c in range(self.game.cols):
                if not self.game.revealed[r][c]:
                    continue
                num = self.game.numbers[r][c]
                if num <= 0:
                    continue
                neighbors = self.get_neighbors(r, c)
                unknown_in_neighbor = []
                flagged = 0
                for nr, nc in neighbors:
                    if self.game.flagged[nr][nc]:
                        flagged += 1
                    elif not self.game.revealed[nr][nc]:
                        unknown_in_neighbor.append((nr, nc))
                if not unknown_in_neighbor:
                    continue
                needed = num - flagged
                if needed > 0:
                    p = needed / len(unknown_in_neighbor)
                else:
                    p = 0.0
                for cell in unknown_in_neighbor:
                    prob[cell] = max(prob[cell], p)

        if prob:
            best = min(prob, key=lambda cell: prob[cell])
            return best
        else:
            return random.choice(unknown_cells)


if __name__ == "__main__":
    root = tk.Tk()
    app = Minesweeper(root)
    root.mainloop()