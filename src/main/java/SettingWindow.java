import sun.security.provider.AuthPolicyFile;

import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

public class SettingWindow extends Frame {

    public static void main(String[] Args) {
        Checkers checkers = new Checkers(new Player(), new Player());
        checkers.printTest();
        SettingWindow win = new SettingWindow();
        win.setVisible(true);
        System.out.print("Window is warking");
    }

    public SettingWindow(){
        Panel panel = new Panel();
        panel.setLayout(new GridLayout(4,1));
        panel.add(new TextField("Choose Player"));
        panel.add(new Button("White"));
        panel.add(new Button("Balck"));
        panel.add(new Button("Exit"));
        add(panel);
        prepareGUI();
    }


    private void prepareGUI(){
        setSize(400,200);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent windowEvent){
                System.exit(0);
            }
        });
    }
}
