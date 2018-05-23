import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;

public class BoardWindow extends Frame{
    private static int width = 800;
    private static int height = 900;
    private static int imageSize = 100;

    private static Image whieteField;
    private static Graphics2D blackField;
    private static Graphics2D whitePiece;
    private static Graphics2D blackPiece;
    private static Graphics2D whiteKing;
    private static Graphics2D blackKing;

    public BoardWindow(SettingWindow.ButtonColour playerType, SettingWindow parent){

        setWindow();
        addWindowListener(new WindowAdapter() {

        });
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent windowEvent){
                parent.setEnabled(true);
                System.out.println("---");
                dispose();
            }
        });

        setGraphic();

        Player player = new Player();
        Player bot = new Player();
        player.printTest();
        bot.printTest();
        Checkers checkers;
        if(playerType == SettingWindow.ButtonColour.White)
            checkers = new Checkers(player,bot);
        else//playerType == Black
            checkers = new Checkers(bot,player);
        checkers.printTest();
    }

    private void setWindow(){
        setVisible(true);
        Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
        setSize(width,height);
        setLocation(dim.width/2-width/2, dim.height/2-height/2 - height/(9*2));
        setResizable(false);

        /*GridLayout layout = new GridLayout(1,1);
        Panel panel = new Panel(layout);
        Label label  = new Label("Game info");
        label.setAlignment(Label.CENTER);
        panel.add(label);

        Panel board = new Panel();
        setBoard(board);
        add(panel);*/
    }

    private void setGraphic(){
       /* byte []image = new byte[imageSize*imageSize];
        whieteField = Toolkit.getDefaultToolkit().createImage(image);
        whieteField.getGraphics().drawOval(0,0,100,100);*/
    }

    private void setBoard(Panel board) {
        //super.paint(whitePiece);
    }
}
