import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import static java.lang.System.exit;

public class SettingWindow extends Frame {
    public enum ButtonClicked{White, Black}
    public static void main(String[] Args) {
        Checkers checkers = new Checkers(new Player(), new Player());
        checkers.printTest();
        SettingWindow win = new SettingWindow();
        win.setVisible(true);
        System.out.print("Window is warking");
    }

    public SettingWindow(){
        setTitle("Checkers");
        Panel panel = new Panel();
        panel.setLayout(new GridLayout(5,1));

        Label text = new Label();
        text.setText("Choose player");
        text.setAlignment(Label.CENTER);

        Button whiteButton = new Button();
        whiteButton.setLabel("White");
        whiteButton.addActionListener(click ->{
            setEnabled(false);
            Object board = new BoardWindow(ButtonClicked.White,
                    this);
        });

        Button blackButton = new Button();
        blackButton.setLabel("Black");
        blackButton.addActionListener(click ->{
            setEnabled(false);
            Object board = new BoardWindow(ButtonClicked.Black,
                    this );
        });

        Label space = new Label();

        Button exitButton = new Button();
        exitButton.setLabel("Exit");
        exitButton.addActionListener(click -> {exit(0);});

        panel.add(text);
        panel.add(whiteButton);
        panel.add(blackButton);
        panel.add(space);
        panel.add(exitButton);
        add(panel);
        setSize(400,200);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent windowEvent){
                exit(0);
            }
        });
    }
}
