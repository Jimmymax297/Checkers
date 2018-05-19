import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import static java.lang.System.exit;
public class BoardWindow extends Frame{
    public BoardWindow(SettingWindow.ButtonClicked playerType, SettingWindow parent){
        setVisible(true);
        Player player = new Player();
        Player bot = new Player();
        Checkers checkers;
        if(playerType == SettingWindow.ButtonClicked.White)
            checkers = new Checkers(player,bot);
        else//playerType == Black
            checkers = new Checkers(bot,player);
        checkers.printTest();
        setSize(400,200);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent windowEvent){
                parent.setEnabled(true);
                dispose();
            }
        });
    }
}
