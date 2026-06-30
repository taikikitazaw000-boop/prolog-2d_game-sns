import javafx.animation.AnimationTimer;
import javafx.application.Application;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;
import javafx.scene.input.KeyCode;
import javafx.scene.input.MouseButton;
import javafx.stage.Stage;
import javafx.scene.image.Image;

import java.util.ArrayList;
import java.util.List;

public class RobotPuzzle extends Application {

    // 定数
    private static final int TILE_SIZE = 40;
    private static final int COLS = 20;
    private static final int ROWS = 15;
    private static final int SCREEN_WIDTH = COLS * TILE_SIZE;
    private static final int SCREEN_HEIGHT = ROWS * TILE_SIZE;

    // ゲームデータ
    private int[][] map = new int[ROWS][COLS];
    private List<Robot> robots = new ArrayList<>();
    
    // ゲーム状態
    private boolean isCleared = false;
    private boolean isGameOver = false;
    private double timeSeconds = 0.0;
    
    // マウス座標
    private double currentMouseX = 0;
    private double currentMouseY = 0;

    // ブロックの画像の変数
    private Image springImage;
    private Image goalImage;

    // JavaFXの部品
    private Stage primaryStage; 
    private GameLoop gameLoop;

    @Override
    public void start(Stage primaryStage) {
        this.primaryStage = primaryStage;
        primaryStage.setTitle("Robot Guidance Puzzle");

        // 画像の読み込み
        try {
            // ※ファイル名が以前と変わっているので注意（bane.pngなど）
            springImage = new Image(getClass().getResourceAsStream("/images/bane.png"));
            goalImage = new Image(getClass().getResourceAsStream("/images/goal.png"));
        } catch (Exception e) {
            System.err.println("【警告】ブロック画像の読み込みに失敗しました。imagesフォルダを確認してください。");
        }

        showHomeScreen();
        primaryStage.show();
    }

    //  画面 1: ホーム画面
    private void showHomeScreen() {
        VBox root = new VBox(20);
        root.setAlignment(Pos.CENTER);
        root.setStyle("-fx-background-color: #222;");

        Label titleLabel = new Label("ROBOT PUZZLE");
        titleLabel.setTextFill(Color.CYAN);
        titleLabel.setFont(Font.font("Arial", FontWeight.BOLD, 50));

        Label descLabel = new Label("Click to Build / Enter for Jump / Wheel for Goal");
        descLabel.setTextFill(Color.WHITE);

        Button btnStage1 = createStageButton("STAGE 1: Basic", 1);
        Button btnStage2 = createStageButton("STAGE 2: Jump", 2);
        Button btnStage3 = createStageButton("STAGE 3: Danger", 3);

        root.getChildren().addAll(titleLabel, descLabel, btnStage1, btnStage2, btnStage3);

        Scene homeScene = new Scene(root, SCREEN_WIDTH, SCREEN_HEIGHT);
        primaryStage.setScene(homeScene);
        
        if (gameLoop != null) gameLoop.stop();
    }

    private Button createStageButton(String text, int level) {
        Button btn = new Button(text);
        btn.setPrefWidth(200);
        btn.setPrefHeight(50);
        btn.setFont(Font.font("Arial", 20));
        btn.setOnAction(e -> startGame(level));
        return btn;
    }

    //  画面 2: ゲーム画面
    private void startGame(int level) {
        isCleared = false;
        isGameOver = false;
        timeSeconds = 0.0;
        robots.clear();
        
        initMap(level);
        robots.add(new Robot(80, 500)); // ロボット初期位置

        Canvas canvas = new Canvas(SCREEN_WIDTH, SCREEN_HEIGHT);
        GraphicsContext gc = canvas.getGraphicsContext2D();

        // マウス座標の記録
        canvas.setOnMouseMoved(e -> {
            currentMouseX = e.getX();
            currentMouseY = e.getY();
        });

        // マウス操作
        canvas.setOnMouseClicked(e -> {
            if (isCleared || isGameOver) return;
            int c = (int) (e.getX() / TILE_SIZE);
            int r = (int) (e.getY() / TILE_SIZE);
            if (isValidEdit(r, c)) {
                if (e.getButton() == MouseButton.PRIMARY) {
                    map[r][c] = (map[r][c] == 1) ? 0 : 1;
                } else if (e.getButton() == MouseButton.MIDDLE) {
                    map[r][c] = (map[r][c] == 3) ? 0 : 3;
                }
            }
        });

        StackPane root = new StackPane(canvas);
        Scene gameScene = new Scene(root);

        // キー操作
        gameScene.setOnKeyPressed(e -> {
            if ((isCleared || isGameOver) && e.getCode() == KeyCode.SPACE) {
                showHomeScreen();
                return;
            }
            if (!isCleared && !isGameOver) {
                if (e.getCode() == KeyCode.ENTER) {
                    int c = (int) (currentMouseX / TILE_SIZE);
                    int r = (int) (currentMouseY / TILE_SIZE);
                    if (isValidEdit(r, c)) {
                        map[r][c] = (map[r][c] == 2) ? 0 : 2;
                    }
                }
            }
        });

        primaryStage.setScene(gameScene);
        gameLoop = new GameLoop(gc);
        gameLoop.start();
        canvas.requestFocus();
    }

    private boolean isValidEdit(int r, int c) {
        if (r < 0 || r >= ROWS || c < 0 || c >= COLS) return false;
        return map[r][c] != 4;
    }

    //  マップ生成
    private void initMap(int level) {
        for (int r = 0; r < ROWS; r++) {
            for (int c = 0; c < COLS; c++) {
                map[r][c] = 0;
            }
        }
        // 外枠(4)
        for (int r = 0; r < ROWS; r++) {
            for (int c = 0; c < COLS; c++) {
                if (r == 0 || r == ROWS - 1 || c == 0 || c == COLS - 1) {
                    map[r][c] = 4;
                }
            }
        }

        switch (level) {
            case 1: // ステージ1
                for (int j = 1; j <= 3; j++) {
                    int y = j * 4;
                    int holeX = 5;
                    if (j == 2) holeX = 15;
                    for (int i = 0; i < COLS; i++) {
                        if (i != holeX) map[y][i] = 4;
                    }
                }
                map[3][17] = 3;
                break;
            case 2: // ステージ2
                for(int r = 5; r < ROWS-1; r++) map[r][10] = 4;
                map[13][8] = 2;
                map[13][18] = 3;
                break;
            case 3: // ステージ3
                for (int c = 8; c <= 12; c++) map[ROWS - 1][c] = 0;
                map[10][5] = 4; map[10][6] = 4;
                map[8][14] = 4; map[8][15] = 4;
                map[6][18] = 3;
                break;
        }
    }

    //  描画メソッド（修正済み）

    private void render(GraphicsContext gc) {
        // ★修正1：背景を真っ黒ではなく、濃い青にしてロボットを見やすくする
        gc.setFill(Color.MIDNIGHTBLUE);
        gc.fillRect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT);

        for (int r = 0; r < ROWS; r++) {
            for (int c = 0; c < COLS; c++) {
                if (map[r][c] == 1) {
                    gc.setFill(Color.GRAY);
                    gc.fillRect(c * TILE_SIZE + 1, r * TILE_SIZE + 1, TILE_SIZE - 2, TILE_SIZE - 2);
                } else if (map[r][c] == 4) {
                    gc.setFill(Color.DARKGRAY);
                    gc.fillRect(c * TILE_SIZE + 1, r * TILE_SIZE + 1, TILE_SIZE - 2, TILE_SIZE - 2);
                } else if (map[r][c] == 2) {
                    if (springImage != null){
                        gc.drawImage(springImage, c * TILE_SIZE, r * TILE_SIZE, TILE_SIZE, TILE_SIZE);
                    } else {
                        gc.setFill(Color.MAGENTA);
                        gc.fillRect(c * TILE_SIZE + 1, r * TILE_SIZE + 1, TILE_SIZE - 2, TILE_SIZE - 2);
                    }
                } else if (map[r][c] == 3) {
                   if (goalImage != null){
                        // ゴールは少し大きめに描画して目立たせる
                        gc.drawImage(goalImage, c * TILE_SIZE, r * TILE_SIZE - 10, TILE_SIZE, TILE_SIZE + 10);
                    } else {
                        gc.setFill(Color.LIMEGREEN);
                        gc.fillRect(c * TILE_SIZE + 1, r * TILE_SIZE + 1, TILE_SIZE - 2, TILE_SIZE - 2);
                    }
                   }
            }
        }

        // ロボット描画
        for (Robot bot : robots) {
            // ★修正2：ロボットの後ろに白い光（ライト）を描いて、背景が何色でも見えるようにする
            gc.setFill(Color.WHITE);
            gc.fillOval(bot.x, bot.y, bot.size, bot.size);

            if (bot.currentImage != null) {
                gc.drawImage(bot.currentImage, bot.x, bot.y, bot.size, bot.size);
            } else {
                gc.setFill(Color.YELLOW);
                gc.fillOval(bot.x, bot.y, bot.size, bot.size);
            }
        }

        // UI描画
        gc.setFill(Color.WHITE);
        gc.setFont(Font.font("Monospaced", FontWeight.BOLD, 25));
        gc.fillText(String.format("TIME: %.2f", timeSeconds), 20, 40);

        if (isCleared) {
            gc.setFill(Color.CYAN);
            gc.setFont(Font.font("Arial", FontWeight.BOLD, 60));
            gc.fillText("STAGE CLEAR!!", 180, 300);
            gc.setFont(Font.font("Arial", 20));
            gc.fillText("Press SPACE to Home", 300, 350);
        } else if (isGameOver) {
            gc.setFill(Color.RED);
            gc.setFont(Font.font("Arial", FontWeight.BOLD, 60));
            gc.fillText("GAME OVER...", 200, 300);
            gc.setFont(Font.font("Arial", 20));
            gc.fillText("Press SPACE to Home", 300, 350);
        }
    }

    public static void main(String[] args) {
        launch(args);
    }

    //  内部クラス：Robot
    class Robot {
        double x, y, vx, vy;
        double size = 30.0;
        double gravity = 0.3;

        private Image[] walkImages = new Image[2];
        private Image[] danceImages = new Image[2];
        Image currentImage = null;
        private int animIndex = 0;
        private long lastAnimTime = 0;

        public Robot(double startX, double startY) {
            this.x = startX;
            this.y = startY;
            this.vx = 2.0;

            try {
                // ファイル名: w1.png, w2.png, d1.png, d2.png に対応
                walkImages[0] = new Image(getClass().getResourceAsStream("/images/w1.png"));
                walkImages[1] = new Image(getClass().getResourceAsStream("/images/w2.png"));
                danceImages[0] = new Image(getClass().getResourceAsStream("/images/d1.png"));
                danceImages[1] = new Image(getClass().getResourceAsStream("/images/d2.png"));
                currentImage = walkImages[0];
            } catch (Exception e) {
                System.err.println("【警告】画像ファイルの読み込みに失敗しました。imagesフォルダを確認してください。");
                currentImage = null;
            }
        }

        public void updateAnimation(boolean isCleared, long now) {
            if (walkImages[0] == null || danceImages[0] == null) return;
            if (now - lastAnimTime < 200_000_000) return; 
            lastAnimTime = now;

            if (isCleared) {
                animIndex = (animIndex + 1) % danceImages.length;
                currentImage = danceImages[animIndex];
            } else {
                if (Math.abs(vx) > 0.1 || Math.abs(vy) > 0.1) {
                    animIndex = (animIndex + 1) % walkImages.length;
                    currentImage = walkImages[animIndex];
                } else {
                    currentImage = walkImages[0];
                    animIndex = 0;
                }
            }
        }

        public void update(int[][] map) {
            if (isCleared || isGameOver) return;
            if (y > SCREEN_HEIGHT) {
                isGameOver = true;
                return;
            }
            vy += gravity;
            y += vy;
            int c = (int) ((x + size / 2) / TILE_SIZE);

            if (vy < 0) {
                int rHead = (int) (y / TILE_SIZE);
                if (rHead >= 0 && c >= 0 && c < COLS) {
                    if (map[rHead][c] != 0 && map[rHead][c] != 3) {
                        y = (rHead + 1) * TILE_SIZE;
                        vy = 2.0;
                    }
                }
            } else if (vy >= 0) {
                int rFoot = (int) ((y + size) / TILE_SIZE);
                if (rFoot >= 0 && rFoot < ROWS && c >= 0 && c < COLS) {
                    if (map[rFoot][c] == 1 || map[rFoot][c] == 4) {
                        y = rFoot * TILE_SIZE - size;
                        vy = 0;
                    } else if (map[rFoot][c] == 2) {
                        y = rFoot * TILE_SIZE - size;
                        vy = -10.0;
                        map[rFoot][c] = 0;
                    } else if (map[rFoot][c] == 3) {
                        isCleared = true;
                    }
                }
            }
            x += vx;
            int nextC = (vx > 0) ? (int) ((x + size) / TILE_SIZE) : (int) (x / TILE_SIZE);
            int currentR = (int) ((y + size / 2) / TILE_SIZE);

            if (currentR >= 0 && currentR < ROWS && nextC >= 0 && nextC < COLS) {
                int tile = map[currentR][nextC];
                if (tile == 1 || tile == 4) {
                    vx *= -1;
                } else if (tile == 2) {
                    vx *= -1;
                    vy = -10.0;
                    map[currentR][nextC] = 0;
                } else if (tile == 3) {
                    isCleared = true;
                }
            }
        }
    }

    
    //  内部クラス：GameLoop
    class GameLoop extends AnimationTimer {
        private GraphicsContext gc;
        private long startTime = 0;

        public GameLoop(GraphicsContext gc) { this.gc = gc; }

        @Override
        public void handle(long now) {
            if (startTime == 0) startTime = now;
            if (!isCleared && !isGameOver) {
                timeSeconds = (now - startTime) / 1_000_000_000.0;
            }
            for (Robot bot : robots) {
                bot.update(map);
                bot.updateAnimation(isCleared, now);
            } 
            render(gc);
        }
    }
}